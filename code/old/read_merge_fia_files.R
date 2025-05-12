rm(list=ls())

# libraries
library(tidyverse)
library(readxl)
library(arrow)



# read in FIA forest type codes
FIA_forest_code <- read_csv("data/FIA/meta/FIA_forest_types.csv") %>% 
  dplyr::select(VALUE, group_label, TYPGRPCD, MEANING)

# read in NE state-level plot data removing unnecessary data add in FIA code
ri_plot<- read_feather("data/FIA/states/FIA_RI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ri_plot <- merge(ri_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING)

ma_plot<- read_feather("fia_data/cleaned_state_data/FIA_MA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ma_plot <- merge(ma_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING)

me_plot<- read_feather("fia_data/cleaned_state_data/FIA_ME.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
me_plot <- merge(me_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING)

ct_plot<- read_feather("fia_data/cleaned_state_data/FIA_CT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ct_plot <- merge(ct_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nh_plot<- read_feather("fia_data/cleaned_state_data/FIA_NH.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nh_plot <- merge(nh_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

vt_plot<- read_feather("fia_data/cleaned_state_data/FIA_VT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
vt_plot <- merge(vt_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

# PNW states
ak_plot<- read_feather("fia_data/cleaned_state_data/FIA_AK.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ak_plot <- merge(ak_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ca_plot<- read_feather("fia_data/cleaned_state_data/FIA_CA.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ca_plot <- merge(ca_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

id_plot<- read_feather("fia_data/cleaned_state_data/FIA_ID.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
id_plot <- merge(id_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

or_plot<- read_feather("fia_data/cleaned_state_data/FIA_OR.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
or_plot <- merge(or_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wa_plot<- read_feather("fia_data/cleaned_state_data/FIA_WA.arrow") %>% 
  filter(alive == 1, forested ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>=5) %>% 
  filter(good_design ==1 | DESIGNCD %in%c(501,502,503,504,505,506)) %>%   
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wa_plot <- merge(wa_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

# Add in rest of States

al_plot<- read_feather("fia_data/cleaned_state_data/FIA_AL.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
al_plot <- merge(al_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ar_plot<- read_feather("fia_data/cleaned_state_data/FIA_AR.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ar_plot <- merge(ar_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

as_plot<- read_feather("fia_data/cleaned_state_data/FIA_AS.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
as_plot <- merge(as_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

az_plot<- read_feather("fia_data/cleaned_state_data/FIA_AZ.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
az_plot <- merge(az_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

co_plot<- read_feather("fia_data/cleaned_state_data/FIA_CO.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
co_plot <- merge(co_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

de_plot<- read_feather("fia_data/cleaned_state_data/FIA_DE.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
de_plot <- merge(de_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

fl_plot<- read_feather("fia_data/cleaned_state_data/FIA_FL.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
fl_plot <- merge(fl_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

fm_plot<- read_feather("fia_data/cleaned_state_data/FIA_FM.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
fm_plot <- merge(fm_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ga_plot<- read_feather("fia_data/cleaned_state_data/FIA_GA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ga_plot <- merge(ga_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

gu_plot<- read_feather("fia_data/cleaned_state_data/FIA_GU.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
gu_plot <- merge(gu_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

hi_plot<- read_feather("fia_data/cleaned_state_data/FIA_HI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
hi_plot <- merge(hi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ia_plot<- read_feather("fia_data/cleaned_state_data/FIA_IA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ia_plot <- merge(ia_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

il_plot<- read_feather("fia_data/cleaned_state_data/FIA_IL.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
il_plot <- merge(il_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

in_plot<- read_feather("fia_data/cleaned_state_data/FIA_IN.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
in_plot <- merge(in_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ks_plot<- read_feather("fia_data/cleaned_state_data/FIA_KS.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ks_plot <- merge(ks_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ky_plot<- read_feather("fia_data/cleaned_state_data/FIA_KY.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ky_plot <- merge(ky_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

la_plot<- read_feather("fia_data/cleaned_state_data/FIA_LA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
la_plot <- merge(la_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

md_plot<- read_feather("fia_data/cleaned_state_data/FIA_MD.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
md_plot <- merge(md_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mh_plot<- read_feather("fia_data/cleaned_state_data/FIA_MH.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mh_plot <- merge(mh_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mi_plot<- read_feather("fia_data/cleaned_state_data/FIA_MI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mi_plot <- merge(mi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mn_plot<- read_feather("fia_data/cleaned_state_data/FIA_MN.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mn_plot <- merge(mn_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mo_plot<- read_feather("fia_data/cleaned_state_data/FIA_MO.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mo_plot <- merge(mo_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mp_plot<- read_feather("fia_data/cleaned_state_data/FIA_MP.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mp_plot <- merge(mp_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ms_plot<- read_feather("fia_data/cleaned_state_data/FIA_MS.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ms_plot <- merge(ms_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

mt_plot<- read_feather("fia_data/cleaned_state_data/FIA_MT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
mt_plot <- merge(mt_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nc_plot<- read_feather("fia_data/cleaned_state_data/FIA_NC.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nc_plot <- merge(nc_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nd_plot<- read_feather("fia_data/cleaned_state_data/FIA_ND.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nd_plot <- merge(nd_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ne_plot<- read_feather("fia_data/cleaned_state_data/FIA_NE.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ne_plot <- merge(ne_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nj_plot<- read_feather("fia_data/cleaned_state_data/FIA_NJ.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nj_plot <- merge(nj_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nm_plot<- read_feather("fia_data/cleaned_state_data/FIA_NM.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nm_plot <- merge(nm_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

nv_plot<- read_feather("fia_data/cleaned_state_data/FIA_NV.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
nv_plot <- merge(nv_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ny_plot<- read_feather("fia_data/cleaned_state_data/FIA_NY.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ny_plot <- merge(ny_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

oh_plot<- read_feather("fia_data/cleaned_state_data/FIA_OH.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
oh_plot <- merge(oh_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ok_plot<- read_feather("fia_data/cleaned_state_data/FIA_OK.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ok_plot <- merge(ok_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

pa_plot<- read_feather("fia_data/cleaned_state_data/FIA_PA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
pa_plot <- merge(pa_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

pr_plot<- read_feather("fia_data/cleaned_state_data/FIA_PR.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
pr_plot <- merge(pr_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

pw_plot<- read_feather("fia_data/cleaned_state_data/FIA_PW.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
pw_plot <- merge(pw_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

sc_plot<- read_feather("fia_data/cleaned_state_data/FIA_SC.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
sc_plot <- merge(sc_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

sd_plot<- read_feather("fia_data/cleaned_state_data/FIA_SD.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
sd_plot <- merge(sd_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

tn_plot<- read_feather("fia_data/cleaned_state_data/FIA_TN.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
tn_plot <- merge(tn_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

tx_plot<- read_feather("fia_data/cleaned_state_data/FIA_TX.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
tx_plot <- merge(tx_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

ut_plot<- read_feather("fia_data/cleaned_state_data/FIA_UT.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
ut_plot <- merge(ut_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

va_plot<- read_feather("fia_data/cleaned_state_data/FIA_VA.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
va_plot <- merge(va_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

vi_plot<- read_feather("fia_data/cleaned_state_data/FIA_VI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
vi_plot <- merge(vi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wi_plot<- read_feather("fia_data/cleaned_state_data/FIA_WI.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wi_plot <- merge(wi_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wv_plot<- read_feather("fia_data/cleaned_state_data/FIA_WV.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wv_plot <- merge(wv_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

wy_plot<- read_feather("fia_data/cleaned_state_data/FIA_WY.arrow") %>% 
  filter(alive == 1, forested ==1, good_design ==1, mult_conds ==0, LAST_YEAR == TRUE, DIA>= 5) %>% 
  select(pid, accepted_bin, BPA,STDAGE, LAT, LON,FORTYPCD,TREEID, TPA_UNADJ, DIA, SPCD, managed, OWNCD)
wy_plot <- merge(wy_plot, FIA_forest_code, by.x = "FORTYPCD", by.y = "VALUE", all.x = TRUE) %>% 
  rename (FIA_label_detail = MEANING) 

# state codes
state_codes <- c("ma", "me", "vt", "ct", "nh", "ri", "ak", "al", "ar", "as", "az", "ca", 
                 "co", "de", "fl", "fm", "ga", "gu", "hi", "ia", "id", "il", "in", "ks", 
                 "ky", "la", "md", "mh", "mi", "mn", "mo", "mp", "ms", "mt", "nc", "nd", 
                 "ne", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "pr", "pw", "sc", 
                 "sd", "tn", "tx", "ut", "va", "vi", "wa", "wi", "wv", "wy")

# List of all dataframes
plot_dfs <- list(ma_plot, me_plot, vt_plot, ct_plot, nh_plot, ri_plot, ak_plot, al_plot, ar_plot, as_plot, az_plot,
                 ca_plot, co_plot, de_plot, fl_plot, fm_plot, ga_plot, gu_plot, hi_plot, ia_plot, id_plot, il_plot, 
                 in_plot, ks_plot, ky_plot, la_plot, md_plot, mh_plot, mi_plot, mn_plot, mo_plot, mp_plot, ms_plot, 
                 mt_plot, nc_plot, nd_plot, ne_plot, nj_plot, nm_plot, nv_plot, ny_plot, oh_plot, ok_plot, or_plot, 
                 pa_plot, pr_plot, pw_plot, sc_plot, sd_plot, tn_plot, tx_plot, ut_plot, va_plot, vi_plot, wa_plot, 
                 wi_plot, wv_plot, wy_plot)

# Add state code column to each dataframe
for (i in seq_along(plot_dfs)) {
  plot_dfs[[i]]$state_code <- state_codes[i]
}

# Merge all dataframes
plot_df_master <- Reduce(function(x, y) merge(x, y, all = TRUE), plot_dfs)

# Remove rows with NA
plot_df_master <- na.omit(plot_df_master)

# Calculate total unique plots per FIA_group
plot_df_master_group<- plot_df_master %>% 
  group_by(group_label) %>% 
  summarise(total_plots = n_distinct(pid))
sum(plot_df_master_group$total_plots)

# Filter by STDAGE and nonstocked
plot_df_master <- plot_df_master %>% 
  group_by(group_label) %>% 
  filter(STDAGE>= quantile(STDAGE,0.5)) %>% 
  filter(!group_label == "Nonstocked") %>% 
  summarise(total_plots = n_distinct(pid))

sum(plot_df_master$total_plots)

plot_df_master <- plot_df_master %>% 
  group_by(group_label) %>% 
  summarise(total_plots = n_distinct(pid))
sum(plot_df_master$total_plots)