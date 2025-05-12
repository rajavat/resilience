library(geosphere)
library(feather)
library(tidyverse)

options(dplyr.print_max = 40)
options(timeout = 1000)

# read in the data
cdoo <- vroom::vroom("FIA/ENTIRE_COND.csv") 
ptoo <- vroom::vroom("FIA/ENTIRE_PLOT.csv") 
troo <- vroom::vroom("FIA/ENTIRE_TREE.csv")
stoo <- vroom::vroom("FIA/ENTIRE_SEEDLING.csv")

# read in the plot id and state lookup table
pid_lookup <- read_csv("fia/raw/plot_id_lookup.csv")
states <- read_csv("fia/fia_state_lookup.csv") 

############ clean up the raw data and select which variables to keep


# condition table
cdo <- cdoo %>%
    select(PLT_CN, CONDID, CONDPROP_UNADJ, PROP_BASIS, COND_STATUS_CD, OWNCD, OWNGRPCD, COND_STATUS_CD, STDAGE, FORINDCD, FORTYPCD, FORTYPCDCALC,
           STDORGCD, TRTCD1_P2A, TRTCD1, TRTCD2, TRTCD3) %>% distinct() %>% 
    mutate(has_cond = TRUE)

# tree table
tro <- troo %>%
    rename(TREEID = CN, PREV_TREEID = PREV_TRE_CN) %>% 
    select(PLT_CN,  CONDID, SUBP, TREE, TREEID, PREV_TREEID,
           STATUSCD, SPGRPCD, SPCD, DIA, DIAHTCD, CCLCD, AGENTCD, CPOSCD, CLIGHTCD,CARBON_AG,
           STOCKING, VOLCFGRS, BHAGE, TOTAGE, MORTCD, STANDING_DEAD_CD, HT, TPA_UNADJ) 

# plot table
pto <- ptoo %>%
    rename(PLT_CN = CN) %>% 
    mutate(PID = paste(UNITCD, STATECD, COUNTYCD, PLOT, DESIGNCD, sep = '_')) %>%
    select(PLT_CN, PID, INVYR, UNITCD, STATECD, COUNTYCD, PLOT, DESIGNCD, KINDCD, REMPER, LAT, LON, MACRO_BREAKPOINT_DIA, DESIGNCD, PLOT_STATUS_CD, PREV_PLT_CN) %>% distinct() %>% 
    mutate(has_plot = TRUE)

# get the lat longs
ll <- pto %>% select(LAT, LON, PLT_CN) %>% distinct() %>% na.omit()


# see which plots have mult conds in the same year, and are thus not completely forested
mult_conds <- cdo %>% select(PLT_CN, CONDID) %>% distinct() %>% group_by(PLT_CN) %>% 
    tally() %>% ungroup %>% filter(n>1) %>% mutate(mult_conds = 1) 

# get the plots with multople lat longs
mult_ll <- pto %>% filter(INVYR < 2000) %>% select(PID, LAT, LON) %>% distinct() %>% group_by(PID) %>% tally() %>% filter(n>1)


# non-forested plots
non_forest <- cdo %>% filter(COND_STATUS_CD != 1) %>% select(PLT_CN) %>% distinct() %>% 
    bind_rows(pto %>% filter(PLOT_STATUS_CD != 1) %>% select(PLT_CN) %>% distinct()) %>% 
    distinct() %>% mutate(non_forest = 1)

# rename the bad designs and the non forest
pt <- pto %>% mutate(good_design = ifelse(DESIGNCD %in%c(1, 311, 312) | (DESIGNCD%in%c(111, 112, 113, 116, 117) & is.na(PREV_PLT_CN) & is.na(REMPER)), 1, 0)) %>%
    left_join(non_forest) %>% mutate(forested = ifelse(is.na(non_forest), 1, 0)) %>% select(-non_forest) 


### read in species names
sp <- read_csv("~/Git/fiadisp/data/fia/cleaned_species_names.csv") %>% 
    mutate(tax_level = ifelse(is.na(word(species, 2)), 1, 2)) 
raw <- read_csv("~/Git/fiadisp/data/fia/raw_species_list.csv")


######################
# combine everything
tr <- tro %>%
    left_join(pt) %>% left_join(cdo) %>% 
    left_join(mult_conds) %>% mutate(mult_conds = ifelse(is.na(mult_conds), 0, 1)) %>% 
    mutate(has_plot = ifelse(is.na(has_plot), FALSE, has_plot)) %>% 
    mutate(has_cond = ifelse(is.na(has_cond), FALSE, has_cond)) %>% 
    left_join(sp)

##### get management/cutting

## get silvilcutre. NOTE that this is not being done by year, so sivliculture at any point excludes the whole plot!
# 80 is silvicultural, 70 is not sure. 3 is cut. removing agentcd = 70
cut_tree_plots <- tro %>% filter(AGENTCD == 80 | STATUSCD == 3) %>% select(PLT_CN) %>% distinct()

# stdorgcd = 1 is evidence of artificail regen. trt codes are anything human, including natural regen since it might be 50% artificial (=40)
cut_cond_plots <- cdo %>% filter(STDORGCD == 1 | TRTCD1_P2A%in%c(10 ,20, 30, 40, 50) | TRTCD1%in%c(10, 20, 30, 40, 50) | TRTCD2%in%c(10, 20, 30, 40, 50) | TRTCD3%in%c(10, 20, 30, 40, 50)) %>%
    select(PLT_CN) %>% distinct()

# combined list of managed plots
cut_plots <- cut_tree_plots %>%
    bind_rows(cut_cond_plots) %>% 
    select(PLT_CN) %>% 
    distinct()

##### read in biome data and lookup tables
blook <- read_csv("misc/biome_lookups.csv")
biomes <- read_csv("fia/raw/FIA_biome_all_plt_cn.csv", num_threads = 48) %>% select(PLT_CN, Resolve_Biome, Resolve_Ecoregion) %>% 
    left_join(blook %>% select(biome, Resolve_Biome)) %>% 
    select(PLT_CN, Resolve_Biome, biome) %>% distinct()

# get plots with multiple biomes
bad_biomes <- biomes %>% filter(PLT_CN%in%tr$PLT_CN) %>% select(PLT_CN, biome) %>% distinct() %>% group_by(PLT_CN) %>% tally() %>% filter(n>1)

# lookup tables to convert codes to names
canopy_lookup <- read_csv("~/Git/dispersion/data/fia_data/metadata/canopy_lookups.csv")
owner_lookup <- read_csv("~/Git/dispersion/data/fia_data/metadata/ownership_lookup.csv")
damage_lookup <- read_csv("~/Git/dispersion/data/fia_data/metadata/damage_lookup.csv")
status_lookup <- read_csv("~/Git/dispersion/data/fia_data/metadata/status_lookup.csv")

##### add silviculture into main data, also clean up stand age
tr_nc <- tr %>% 
    mutate(managed = ifelse(PLT_CN%in%cut_plots$PLT_CN, 1, 0)) %>%
    mutate(STDAGE = ifelse(STDAGE < 0, NA, STDAGE)) %>% 
    filter(!is.na(STATUSCD)) %>%
    filter(!PLT_CN%in%bad_biomes$PLT_CN) %>% 
    mutate(alive = ifelse(STATUSCD == 1, 1, 0)) %>% 
    left_join(biomes)

# add in lookup tables, do some additional calulations (biomass, size_class, plot_type)
tr_look <- tr_nc %>% 
    left_join(canopy_lookup) %>% 
    left_join(owner_lookup) %>% 
    left_join(damage_lookup) %>% 
    left_join(status_lookup) %>% 
    rename(height = HT) %>% 
    mutate(biomass = CARBON_AG*2) %>% 
    mutate(size_class = ifelse(DIA < 0.5, 1, ifelse(DIA < 7, 2, 3))) %>% 
    mutate(PLOT_TYPE = ifelse(abs(TPA_UNADJ - 6.018) < 0.025, "SUBP",
                              ifelse(abs(TPA_UNADJ - 75) < 1, "MICRO", 
                                     ifelse(abs(TPA_UNADJ - 1) < 0.05, "MACRO", "OTHER")))) %>% 
    mutate(BPA = biomass * TPA_UNADJ) %>% 
    mutate(USE = ifelse(alive == 1 & forested == 1 & managed == 0 & good_design == 1 & mult_conds == 0, TRUE, FALSE)) %>% 
    left_join(pid_lookup) %>% 
    select(pid, names(.)) %>% 
    filter(INVYR <= 2024, INVYR > 1900) %>% 
    left_join(states)

# calculate the min/max year per plot
year_lims <- tr_look %>% select(UNITCD, STATECD, COUNTYCD, PLOT, INVYR) %>% distinct() %>% group_by(UNITCD, STATECD, COUNTYCD, PLOT) %>% summarize(max_year = max(INVYR), min_year = min(INVYR)) %>% ungroup

# add in indicator if plot is first/last 
tr_final <- tr_look %>% left_join(year_lims) %>% mutate(LAST_YEAR = I(INVYR == max_year), FIRST_YEAR = I(INVYR == min_year))

ll_lookup <- tr_final %>% select(LAT, LON) %>% distinct() %>% arrange(LAT, LON) %>% mutate(ll_id = 1:nrow(.))

rm(list = setdiff(ls(), c("tr_final", "ss", "nspp", "ssum")))
gc()

ss <- tr_final %>% filter(USE == 1) %>% filter(LAST_YEAR = TRUE) %>% filter(DIA >=5) %>% filter(!is.na(STDAGE)) %>% filter(STDAGE > 50)

nspp <- ss %>% filter(!is.na(SPCD)) %>% select(pid, SPCD) %>% distinct() %>% group_by(pid) %>% tally() %>% filter(n<10) 

ssum <- ss %>% group_by(pid, accepted_bin) %>% summarize(M = mean(biomass), A = sum(TPA_UNADJ)) %>% ungroup

ss <- ss %>% filter(!pid%in%nspp$pid)

ssum %>% ggplot(aes(x = log(A), y = log(M)))+geom_smooth(method = "lm")

write_csv(ll_lookup %>% slice(1:100), "~/RDS/forestcom/fia_data/TEST_lat_long_lookup.csv")

tr_final <- tr_final %>% left_join(ll_lookup)

plot_ll_id <- tr_final %>% select(pid, ll_id, LAT, LON) %>% distinct()
write_csv(plot_ll_id, "~/RDS/forestcom/fia_data/plot_ll_id.csv")

# splite up the states and write as arrow files
for(i in 1:nrow(states)){
    print(i)
    tmp <- tr_final %>% filter(STATECD == states$STATECD[i])
    
    if(nrow(tmp)>0){
        arrow::write_feather(tmp, paste0("~/RDS/forestcom/fia_data/cleaned_state_data/FIA_",states$STATE_ABBRV[i],".arrow"))
    }
}
        
comp <- read_csv("~/Git/dispersion/data/composite_FIA_alastair.csv")

tr <- read.tree("~/nas/Dan/Git/functional_disp/data/cleaned_data/phy_tree_BGCI_full.newick")

dt <- read_csv("~/RDS/forestcom/trait_data/Estimated_trait_table_with_monos.csv") %>% 
    select(accepted_bin) %>% distinct() %>% 
    mutate(accepted_bin = gsub(" " , "_", accepted_bin))

sum(!dt$accepted_bin%in%tr$tip.label)

dt2 <- read_feather("/home/dsenn/Git/functional_disp/paper/for_revision/data/raw_data/TRY_trait_data.feather")

tax <- read_csv("~/RDS/forestcom/trait_data/ANGIO_GYMNO_lookup.csv")

monocots <- c("Acorales", "Alismatales", "Arecales", "Asparagales", "Commelinales", "Dioscoreales", "Liliales", "Pandanales", "Petrosaviales", "Poales", "Zingiberales")
ferns <- c("Lophosoria", "Metaxya", "Sphaeropteris", "Alsophila", "Nephelea", "Trichipteris", "Cyathea" ,"Cnemidaria", "Dicksonia", "Cystodium", "Thyrsopteris", "Culcita", "Cibotium")

monocot_genera <- read_csv("/home/dsenn/Git/old/Tree_diversity_back/data/raw_data/monocot_genera.csv")

spp <- dt %>% select(accepted_bin) %>% bind_rows(dt2 %>% select(accepted_bin)) %>% bind_rows(tibble(accepted_bin = tr$tip_label)) %>% distinct()

keepsp <- dt %>% select(accepted_bin) %>% bind_rows(dt2 %>% select(accepted_bin)) %>% bind_rows(tibble(accepted_bin = tr$tip_label)) %>% distinct() %>%
    mutate(tax_genus = word(accepted_bin, 1)) %>% 
    filter(!tax_genus%in%monocot_genera$tax_genus) %>%
    select(accepted_bin, tax_genus) %>% 
    left_join(tax %>% select(accepted_bin, order, group, genus, family)) %>%
    filter(!order%in%monocots, !genus%in%ferns, !tax_genus%in%ferns, !family%in%c("Osmundaceae"), group%in%c("Angiosperms", "Gymnosperms")) %>% 
    select(accepted_bin)

tax2 <- tax %>% filter(accepted_bin%in%spp$accepted_bin) %>% mutate(mono_fern = ifelse(accepted_bin%in%keepsp$accepted_bin, FALSE, TRUE))

write_csv(tax2, "~/RDS/forestcom/trait_data/taxonomic_information.csv")
