# INITIALISE ———————————————————————————————————————————————————————————————————
library(arrow)
library(stringr)
library(future)
library(furrr)

options(datatable.print.nrows = 40)
options(timeout = 1000)
nCores <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = nCores)

# READ IN DATA —————————————————————————————————————————————————————————————————
condData <- data.table::fread("data/FIA/ENTIRE_COND.csv",
                              select = c("PLT_CN",
                                         "CONDID",
                                         "CONDPROP_UNADJ",
                                         "PROP_BASIS",
                                         "COND_STATUS_CD",
                                         "OWNCD",
                                         "OWNGRPCD",
                                         "STDAGE",
                                         "FORINDCD",
                                         "FORTYPCD",
                                         "FORTYPCDCALC",
                                         "STDORGCD",
                                         "TRTCD1_P2A",
                                         "TRTCD1",
                                         "TRTCD2",
                                         "TRTCD3"),
                              nThread = nCores)

plotData <- data.table::fread("data/FIA/ENTIRE_PLOT.csv",
                              select = c("CN",
                                         "INVYR",
                                         "UNITCD",
                                         "STATECD",
                                         "COUNTYCD",
                                         "PLOT",
                                         "DESIGNCD",
                                         "KINDCD",
                                         "REMPER",
                                         "LAT",
                                         "LON",
                                         "MACRO_BREAKPOINT_DIA",
                                         "PLOT_STATUS_CD",
                                         "PREV_PLT_CN"),
                              nThread = nCores)

treeData <- data.table::fread("data/FIA/ENTIRE_TREE.csv",
                              select = c("PLT_CN",
                                         "CONDID",
                                         "SUBP",
                                         "TREE",
                                         "CN",
                                         "PREV_TRE_CN",
                                         "STATUSCD",
                                         "SPGRPCD",
                                         "SPCD",
                                         "DIA",
                                         "DIAHTCD",
                                         "CCLCD",
                                         "AGENTCD",
                                         "CPOSCD",
                                         "CLIGHTCD",
                                         "CARBON_AG",
                                         "STOCKING",
                                         "VOLCFGRS",
                                         "BHAGE",
                                         "TOTAGE",
                                         "MORTCD",
                                         "STANDING_DEAD_CD",
                                         "HT",
                                         "TPA_UNADJ"),
                              nThread = nCores)

# read in meta lookup files
lookupFiles <- list(
  pidLookup = "data/FIA/meta/plot_id_lookup.csv",
  statesLookup = "data/FIA/meta/fia_state_lookup.csv",
  canopyLookup = "data/FIA/meta/canopy_lookups.csv",
  ownerLookup = "data/FIA/meta/ownership_lookup.csv",
  damageLookup = "data/FIA/meta/damage_lookup.csv",
  statusLookup = "data/FIA/meta/status_lookup.csv",
  biomeLookup = "data/FIA/meta/biome_lookups.csv",
  speciesData = "data/FIA/meta/cleaned_species_names.csv",
  biomeData = "data/FIA/meta/FIA_biome_all_plt_cn.csv"
)

lookupTables <- lapply(lookupFiles,
                       data.table::fread, nThread = nCores)

# process biome data
biomeData <- lookupTables$biomeData[, .(PLT_CN, 
                                        Resolve_Biome, 
                                        Resolve_Ecoregion)]
biomeData <- merge(biomeData,
                   lookupTables$biomeLookup[, .(biome, 
                                                Resolve_Biome)], 
                   by = "Resolve_Biome")

biomeData <- unique(biomeData[, .(PLT_CN, 
                                  Resolve_Biome,
                                  biome)])

# process species data
speciesData <- lookupTables$speciesData
speciesData[, taxLevel := ifelse(is.na(word(species, 2)), 1, 2)]

# PROCESSING ———————————————————————————————————————————————————————————————————

# condition table
condData <- unique(condData)
condData[, hasCond := TRUE]

# tree table
setnames(treeData, 
         c("CN", "PREV_TRE_CN"),
         c("TREEID", "PREV_TREEID"))

# plot table
setnames(plotData,
         "CN",
         "PLT_CN")

plotData[, PID := paste(UNITCD, 
                        STATECD, 
                        COUNTYCD, 
                        PLOT, 
                        DESIGNCD, 
                        sep = '_')]

plotData <- plotData[, .(PLT_CN, 
                         PID, 
                         INVYR, 
                         UNITCD, 
                         STATECD, 
                         COUNTYCD,
                         PLOT, 
                         DESIGNCD, 
                         KINDCD, 
                         REMPER, 
                         LAT, 
                         LON,
                         MACRO_BREAKPOINT_DIA,
                         PLOT_STATUS_CD,
                         PREV_PLT_CN)]
plotData <- unique(plotData)
plotData[, hasPlot := TRUE]

# FILTERING - identify multi-condition plots
condMulti <- condData[, .(count = .N), 
                      by = PLT_CN][count > 1,
                                   .(PLT_CN, cdMult = 1)]

# identify non-forested plots
nonForest <- rbindlist(list(
  condData[COND_STATUS_CD != 1, .(PLT_CN, cdNf = 1)],
  plotData[PLOT_STATUS_CD != 1, .(PLT_CN, cdNf = 1)]
), use.names = TRUE)
nonForest <- unique(nonForest)

# process design codes and forest status
plotData[, goodDesign := fifelse(DESIGNCD %in% c(1, 311, 312) |
                                   (DESIGNCD %in% c(111, 112, 113, 116, 117) &
                                      is.na(PREV_PLT_CN) & is.na(REMPER)), 1, 0)]
plotData <- merge(plotData, 
                  nonForest, 
                  by = "PLT_CN", 
                  all.x = TRUE)

plotData[, forested := fifelse(is.na(cdNf), 1, 0)]
plotData[, cdNf := NULL]

# identify managed plots from tree data
cutTreePlots <- treeData[AGENTCD == 80 | STATUSCD == 3, .(PLT_CN)]
cutTreePlots <- unique(cutTreePlots)

# identify managed plots from condition data
cutCondPlots <- condData[STDORGCD == 1 |
                           TRTCD1_P2A %in% c(10, 20, 30, 40, 50) |
                           TRTCD1 %in% c(10, 20, 30, 40, 50) |
                           TRTCD2 %in% c(10, 20, 30, 40, 50) |
                           TRTCD3 %in% c(10, 20, 30, 40, 50), .(PLT_CN)]
cutCondPlots <- unique(cutCondPlots)

# combine managed plots
cutPlots <- rbindlist(list(cutTreePlots, cutCondPlots), use.names = TRUE)
cutPlots <- unique(cutPlots)

# COMBINE DATA —————————————————————————————————————————————————————————————————
# merge tree, plot, and condition data
treeData <- merge(treeData, plotData, 
                  by = "PLT_CN", all.x = TRUE)
treeData <- merge(treeData, condData, 
                  by = c("PLT_CN", "CONDID"), all.x = TRUE)
treeData <- merge(treeData, condMulti, 
                  by = "PLT_CN", all.x = TRUE)

treeData[, cdMult := fifelse(is.na(cdMult), 0, 1)]
treeData[, `:=`(
  hasPlot = fifelse(is.na(hasPlot), FALSE, hasPlot),
  hasCond = fifelse(is.na(hasCond), FALSE, hasCond)
)]

# merge species data
treeData <- merge(treeData, speciesData, 
                  by = "SPCD", all.x = TRUE)

# identify plots with multiple biomes
multBiomePlots <- biomeData[PLT_CN %in% treeData$PLT_CN, 
                            .(count = .N), by = PLT_CN][count > 1, PLT_CN]

# add silviculture and filter data
treeData[, `:=`(
  managed = fifelse(PLT_CN %in% cutPlots$PLT_CN, 1, 0),
  STDAGE = fifelse(STDAGE < 0, NA_real_, STDAGE),
  alive = fifelse(STATUSCD == 1, 1, 0)
)]

# filter out rows with missing status and multiple biomes
treeData <- treeData[!is.na(STATUSCD) & !PLT_CN %in% multBiomePlots]
treeData <- merge(treeData, biomeData, by = "PLT_CN", all.x = TRUE)

# add lookup data
treeData <- merge(treeData, lookupTables$canopyLookup, 
                  by = "CCLCD", all.x = TRUE)
treeData <- merge(treeData, lookupTables$ownerLookup, 
                  by = "OWNCD", all.x = TRUE)
treeData <- merge(treeData, lookupTables$damageLookup, 
                  by = "AGENTCD", all.x = TRUE)
treeData <- merge(treeData, lookupTables$statusLookup, 
                  by = "STATUSCD", all.x = TRUE)
setnames(treeData, "HT", "height")

# calculate derived variables
treeData[, `:=`(
  biomass = CARBON_AG * 2,
  sizeClass = fifelse(DIA < 0.5, 1, fifelse(DIA < 7, 2, 3)),
  plotType = fifelse(abs(TPA_UNADJ - 6.018) < 0.025, "SUBP",
                     fifelse(abs(TPA_UNADJ - 75) < 1, "MICRO",
                             fifelse(abs(TPA_UNADJ - 1) < 0.05,
                                     "MACRO", "OTHER"))),
  BPA = CARBON_AG * 2 * TPA_UNADJ,
  USE = alive == 1 & forested == 1 & goodDesign == 1 & cdMult == 0
)]

# merge with PID lookup
setnames(lookupTables$pidLookup, "pid", "PID")
treeData <- merge(treeData, lookupTables$pidLookup,
                  by = "PID", all.x = TRUE)

# filter by year
treeData <- treeData[INVYR <= 2024 & INVYR > 1900]
treeData <- merge(treeData, lookupTables$statesLookup, 
                  by = "STATECD", all.x = TRUE)

# create coordinate lookup
llLookup <- unique(treeData[, .(LAT, LON)])
setorder(llLookup, LAT, LON)
llLookup[, llId := .I]

# calculate year limits per plot
yearLimits <- treeData[, .(maxYear = max(INVYR), minYear = min(INVYR)),
                       by = .(UNITCD, STATECD, COUNTYCD, PLOT)]

# add first/last year indicators
treeData <- merge(treeData, yearLimits, 
                  by = c("UNITCD", "STATECD", "COUNTYCD", "PLOT"))
treeData[, `:=`(
  lastYear = INVYR == maxYear,
  firstYear = INVYR == minYear
)]
treeData <- merge(treeData, llLookup, 
                  by = c("LAT", "LON"))

# EXPORTS  —————————————————————————————————————————————————————————————————————
# save coordinate lookup
llLookup <- unique(treeData[, .(PID, llId, LAT, LON)])
data.table::fwrite(llLookup, "llLookup.csv")

# function to write state data in parallel
writeStateData <- function(stateCode) {
  stateAbbr <- lookupTables$statesLookup[STATECD == stateCode, STATE_ABBRV]
  stateTrees <- treeData[STATECD == stateCode]
  
  if (nrow(stateTrees) > 0) {
    arrow::write_feather(stateTrees, paste0("~/FIA_", stateAbbr, ".arrow"))
  }
  return(NULL)
}
uniqueStates <- unique(lookupTables$statesLookup$STATECD)
future_map(uniqueStates, writeStateData, .options = furrr_options(seed = TRUE))

plan(sequential)
