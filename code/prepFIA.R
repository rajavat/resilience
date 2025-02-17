# INITIALISE ————————————————————————————————————————————————
library(geosphere)
library(feather)
library(tidyverse)

options(dplyr.print_max = 40)
options(timeout = 1000)

# READ IN DATA ——————————————————————————————————————————————
cdoo <- vroom::vroom("FIA/ENTIRE_COND.csv", 
                     num_threads = 11, 
                     guess_max = 250000) 
ptoo <- vroom::vroom("FIA/ENTIRE_PLOT.csv", 
                     num_threads = 11, 
                     guess_max = 100000) 
stoo <- vroom::vroom("FIA/ENTIRE_SEEDLING.csv", 
                     num_threads = 11, 
                     guess_max = 100000)
troo <- vroom::vroom("FIA/ENTIRE_TREE.csv", 
                     num_threads = 11, 
                     guess_max = 250000)

# read in meta lookup files
pid_lookup    <- read_csv("FIA/meta/plot_id_lookup.csv")
states_lookup <- read_csv("FIA/meta/fia_state_lookup.csv")
canopy_lookup <- read_csv("FIA/meta/canopy_lookups.csv")
owner_lookup  <- read_csv("FIA/meta/ownership_lookup.csv")
damage_lookup <- read_csv("FIA/meta/damage_lookup.csv")
status_lookup <- read_csv("FIA/meta/status_lookup.csv")
biome_lookup  <- read_csv("FIA/meta/biome_lookups.csv")

# read in biome data
biomes <- read_csv("FIA/meta/FIA_biome_all_plt_cn.csv") %>% 
  select(PLT_CN, Resolve_Biome, Resolve_Ecoregion) %>%
  left_join(biome_lookup %>%
              select(biome, Resolve_Biome)) %>% 
  select(PLT_CN, Resolve_Biome, biome) %>%
  unique()

# read in species names 
sp_names <- read_csv("FIA/meta/cleaned_species_names.csv") %>% 
  mutate(tax_level = ifelse(is.na(word(species, 2)), 1, 2))

raw_names <- read_csv("FIA/meta/raw_species_list.csv")

# CONDITION TABLE ————————————————————————————————————————————

cdo <- cdoo %>% 
  select(PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, 
         COND_STATUS_CD, OWNCD, OWNGRPCD, COND_STATUS_CD, 
         STDAGE, FORINDCD, FORTYPCD, FORTYPCDCALC, STDORGCD, 
         TRTCD1_P2A, TRTCD1, TRTCD2, TRTCD3) %>%
  unique() %>%
  
  mutate(has_cond = TRUE)

# TREE TABLE ————————————————————————————————————————————————

tro <- troo %>% 
  rename(TREEID = CN, PREV_TREEID = PREV_TRE_CN) %>% 
  select(PLT_CN, CONDID, SUBP, TREE, TREEID, PREV_TREEID,
         STATUSCD, SPGRPCD, SPCD, DIA, DIAHTCD, CCLCD,
         AGENTCD, CPOSCD, CLIGHTCD, CARBON_AG, STOCKING, 
         VOLCFGRS, BHAGE, TOTAGE, MORTCD, STANDING_DEAD_CD,
         HT, TPA_UNADJ)

# PLOT TABLE ————————————————————————————————————————————————

pto <- ptoo %>% 
  rename(PLT_CN = CN) %>%
  mutate(PID = paste(UNITCD, STATECD, COUNTYCD, 
                     PLOT, DESIGNCD, sep = '_')) %>%
  select(PLT_CN, PID, INVYR, UNITCD, STATECD, COUNTYCD, 
         PLOT, DESIGNCD, KINDCD, REMPER, LAT, LON, 
         MACRO_BREAKPOINT_DIA, DESIGNCD, PLOT_STATUS_CD, 
         PREV_PLT_CN) %>%
  unique() %>%
  mutate(has_plot = TRUE)

# separate laditudes + longitudes
ll <- pto %>% 
  select(LAT, LON, PLT_CN) %>% 
  unique() %>% 
  na.omit()


# FILTERING ———————————————————————————————————————————————

# filter plots that aren't completely forested
cd_mult <- cdo %>% 
  select(PLT_CN, CONDID) %>% 
  unique() %>% 
  group_by(PLT_CN) %>%
  tally() %>%
  ungroup %>%
  filter(n > 1) %>%
  mutate(cd_mult = 1)

# filter plots with multiple coordinates
pt_mult <- pto %>%
  filter(INVYR < 2000) %>%
  select(PID, LAT, LON) %>% 
  distinct() %>%
  group_by(PID) %>% 
  tally() %>% 
  filter(n > 1)

# filter non-forested plots 
cd_nf <- cdo %>% 
  filter(COND_STATUS_CD != 1) %>%
  select(PLT_CN) %>% 
  unique() %>% 
  bind_rows(pto %>% filter(PLOT_STATUS_CD != 1) 
            %>% select(PLT_CN) %>% unique()) %>% 
  unique() %>% 
  mutate(cd_nf = 1)

# rename bad design and non forested 
pt <- pto %>% 
  mutate(good_design = 
           ifelse(DESIGNCD %in%c(1, 311, 312) 
                  | (DESIGNCD%in%c(111, 112, 113, 116, 117) 
                     & is.na(PREV_PLT_CN) & is.na(REMPER)), 1, 0)) %>%
  left_join(cd_nf) %>% 
  mutate(forested = ifelse(is.na(cd_nf), 1, 0)) %>% 
  select(-cd_nf) 

# COMBINE EVERYTHING ————————————————————————————————————————

tr <- tro %>%
  left_join(pt) %>% 
  left_join(cdo) %>%
  left_join(cd_mult) %>%
  mutate(mult_conds = ifelse(is.na(cd_mult), 0, 1)) %>% 
  mutate(has_plot = ifelse(is.na(has_plot), FALSE, has_plot)) %>% 
  mutate(has_cond = ifelse(is.na(has_cond), FALSE, has_cond)) %>% 
  left_join(sp_names)

# SILVICULTURING ———————————————————————————————————————————

# get silviculture —
## NOTE that this is not being done by year
## so sivliculture at any point excludes the whole plot!
## 80 is silvicultural 
## 70 is not sure, 3 is cut, removing agentcd = 70

cut_tree_plots <- tro %>% 
  filter(AGENTCD == 80 | STATUSCD == 3) %>%
  select(PLT_CN) %>%
  unique()

# get artificial regen —
## stdorgcd = 1 is evidence of artificial regen
## trt codes are anything human
## including natural regen since it might be 50% artificial (=40)

cut_cond_plots <- cdo %>% 
  filter(STDORGCD == 1 | 
           TRTCD1_P2A%in%c(10 ,20, 30, 40, 50) | 
           TRTCD1%in%c(10, 20, 30, 40, 50) | 
           TRTCD2%in%c(10, 20, 30, 40, 50) | 
           TRTCD3%in%c(10, 20, 30, 40, 50)) %>%
  select(PLT_CN) %>% 
  unique()

# combined list of managed plots
cut_plots <- cut_tree_plots %>%
  bind_rows(cut_cond_plots) %>% 
  select(PLT_CN) %>% 
  unique()

# get plots with multiple biomes
mult_plots <- biomes %>% 
  filter(PLT_CN%in%tr$PLT_CN) %>% 
  select(PLT_CN, biome) %>% 
  unique() %>% 
  group_by(PLT_CN) %>% 
  tally() %>% 
  filter(n>1)

## add silviculture into main data
## also clean up stand age
tr_nc <- tr %>% 
  mutate(managed = ifelse(PLT_CN%in%cut_plots$PLT_CN, 1, 0)) %>%
  mutate(STDAGE = ifelse(STDAGE < 0, NA, STDAGE)) %>% 
  filter(!is.na(STATUSCD)) %>%
  filter(!PLT_CN%in%mult_biomes$PLT_CN) %>% 
  mutate(alive = ifelse(STATUSCD == 1, 1, 0)) %>% 
  left_join(biomes)

## determine coordinates
ll_lookup <- tr_final %>%
  select(LAT, LON) %>%
  distinct() %>% 
  arrange(LAT, LON) %>% 
  mutate(ll_id = 1:nrow(.))

# calculate the min/max year per plot
year_lims <- tr_look %>% 
  select(UNITCD, STATECD, 
         COUNTYCD, PLOT, 
         INVYR) %>% 
  distinct() %>% 
  group_by(UNITCD, STATECD, 
           COUNTYCD, PLOT) %>% 
  summarize(max_year = max(INVYR),
            min_year = min(INVYR)) %>% ungroup


# COMBINE DATA ———————————————————————————————————————————

# add in lookup tables, do some additional calulations 
## (biomass, size_class, plot_type)

tr_look <- tr_nc %>% 
  left_join(canopy_lookup) %>% 
  left_join(owner_lookup) %>% 
  left_join(damage_lookup) %>% 
  left_join(status_lookup) %>% 
  rename(height = HT) %>% 
  mutate(biomass = CARBON_AG*2) %>% 
  mutate(size_class = ifelse(DIA < 0.5, 1, 
                             ifelse(DIA < 7, 2, 3))) %>% 
  mutate(PLOT_TYPE = ifelse(abs(TPA_UNADJ - 6.018) < 0.025,
                            "SUBP",
                            ifelse(abs(TPA_UNADJ - 75) < 1,
                                   "MICRO", 
                                   ifelse(abs(TPA_UNADJ - 1) < 0.05,
                                          "MACRO", "OTHER")))) %>% 
  mutate(BPA = biomass * TPA_UNADJ) %>% 
  mutate(USE = ifelse(alive == 1 &
                        forested == 1 &
                        good_design == 1 &
                        cd_mult == 0, 
                      TRUE, FALSE)) %>% 
  
  left_join(pid_lookup) %>% 
  select(pid, names(.)) %>% 
  filter(INVYR <= 2024,
         INVYR > 1900) %>% 
  left_join(states)

# add in indicators if plot is first/last year + lat/longs
tr_final <- tr_look %>% 
  left_join(year_lims) %>% 
  mutate(LAST_YEAR = I(INVYR == max_year),
         FIRST_YEAR = I(INVYR == min_year)) %>%
  left_join(ll_lookup)

# EXPORTS ————————————————————————————————————————————————————

ll_lookup <- tr_final %>%
  select(pid, ll_id, 
         LAT, LON) %>% 
  unique()
write_csv(ll_lookup, "~/ll_lookup.csv")

# split up the states and write as arrow files

for(i in 1:nrow(states_lookup)){
  print(i)
  tmp <- tr_final %>%
    filter(STATECD == 
             states_lookup$STATECD[i])
  
  if(nrow(tmp)>0){
    arrow::write_feather(tmp,
                         paste0("~/FIA_",
                                states_lookup$STATE_ABBRV[i],
                                ".arrow"))
  }
}
