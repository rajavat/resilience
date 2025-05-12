library(tidyverse)
library(arrow)

library(caret)
library(ClustOfVar)
library(car)
library(MuMIn)
library(nlme)
library(corrplot)

library(sp)
library(spdep)

# ——————————————————————————————————————————————————————————————————————————————

# load in FD indices
dataFD <- read.csv("data/trait/FD_fundiversity.csv")

# load in residuals
stability <- read.csv("data/GEE/stability.csv")

# merge residuals with FD per PID
master <- merge(stability, dataFD, by = "PID")
master <- master %>%
  select(PID, meanEVI, 
         sdResidEVI, coeffVarEVI,
         FDis, FDiv,
         FEve, RaoQ)

# load in diversity metrics
diversity <- read.csv("data/trait/diversityMetrics.csv")

# merge diversity metrics with FD per PID
master <- merge(master, diversity, by = "PID") 
master <- master %>%
  select(-X)

# load in FIA
dataFIA <- read_feather("data/FIA/FIAmaster.arrow")

# filter subset of cols from FIA
summaryFIA <- dataFIA %>%
  group_by(PID) %>%
  summarise(
    biome = unique(biome),
    ownership = unique(ownership),
    status = unique(status),
    managed = unique(managed),
    group_label = unique(group_label),
    llId = unique(llId),
    LAT = unique(LAT),
    LON = unique(LON),
  )

# merge master with FIA cols
master <- merge(master, summaryFIA, by = "PID")

# check for duplicate coords based on lat long ID
duplicate <- duplicated(master$llId) | duplicated(master$llId, fromLast = TRUE)
duplicate <- master[duplicate, ]

# keep first occurrence for duplicates
master <- master %>%
  distinct(llId, .keep_all = TRUE)
rm(duplicate)

# rename for consistency with composite
master <- rename(master, ll_id = llId)

# ——————————————————————————————————————————————————————————————————————————————
# load in a sample of composite data to check col names
sampleComp <- read.csv("data/Composite/raw/composite_FIA_variables.csv", nrows = 2)
colnames(sampleComp)

# load in composite data
dataComp <- data.table::fread("data/Composite/raw/composite_FIA_variables.csv",
                              select = c("ll_id", "lat", "lon",
                                         
                                # climate variables
                                "CHELSA_BIO_Annual_Mean_Temperature",
                                "CHELSA_BIO_Annual_Precipitation",
                                "CHELSA_BIO_Precipitation_Seasonality",
                                "CHELSA_BIO_Mean_Temperature_of_Coldest_Quarter",
                                "CHELSA_BIO_Mean_Temperature_of_Driest_Quarter",
                                "CHELSA_BIO_Precipitation_of_Coldest_Quarter",
                                "CHELSA_BIO_Precipitation_of_Driest_Quarter",
                                "CHELSA_BIO_Temperature_Annual_Range",
                                "CHELSA_BIO_Mean_Diurnal_Range",
                                "CHELSA_exBIO_GrowingSeasonLength", 
                                "CHELSA_exBIO_FrostChangeFrequency",
                                "CHELSA_exBIO_SnowCoverDays",
                                "CHELSA_EXTRM15_Max_Temperature_of_Warmest_Month",
                                "CHELSA_EXTRM15_Min_Temperature_of_Coldest_Month",
                                "CHELSA_EXTRM15_Precipitation_of_Driest_Quarter",
                                "CGIAR_Aridity_Index",
                                "CGIAR_PET",
                                "WorldClim2_SolarRadiation_AnnualSD",
                                "WorldClim2_WindSpeed_AnnualMean",
                                
                                # extreme events & disturbance regimes
                                "EsaCci_BurntAreasProbability",
                                "GFAD_FractionOfRegrowthForest_downsampled50km",
                                "GFAD_regrowthForestAge_Mean_downsampled50km",
                                "GFAD_regrowthForestAge_SD_downsampled50km",
                                
                                # anthropogenic factors
                                "LesivEtAl_NaturallyRegeneratingForests_wManagement", 
                                "LesivEtAl_PlantedForests", 
                                "ConsensusLandCover_Human_Development_Percentage",
                                "CSP_Global_Human_Modification",
                                "GranthamEtAl_ForestLandscapeIntegrityIndex",
                                "WCS_Human_Footprint_2009",
                                
                                # topography
                                "EarthEnvTopoMed_Eastness",
                                "EarthEnvTopoMed_Elevation",
                                "EarthEnvTopoMed_Northness",
                                "EarthEnvTopoMed_Slope",
                                "SG_Depth_to_bedrock",
                                
                                # soil properties
                                "SG_Bulk_density_015cm",
                                "SG_CEC_015cm",
                                "SG_Clay_Content_015cm",
                                "SG_Coarse_fragments_015cm",
                                "SG_SOC_Content_015cm",
                                "SG_Sand_Content_015cm",
                                "SG_Silt_Content_015cm",
                                "SG_Soil_pH_H2O_015cm",
                                
                                # ecosystem structure & function
                                "CrowtherLab_SoilMoisture_intraAnnualSD_downsampled10km",
                                "EarthEnvCloudCover_intraAnnualSD",
                                "CrowtherLab_RootMassFraction",
                                "CrowtherLab_IntactLandscapes",
                                "CrowtherLab_Tree_Density"))

# ——————————————————————————————————————————————————————————————————————————————

# merge with composite data based on lat long IDs
master <- merge(master, dataComp, by = "ll_id") 

master <- master %>%
  drop_na()

# convert factors
master <- master %>%
  mutate(
    biome = factor(biome),
    managed = factor(managed),
    ownership = factor(ownership),
    status = factor(status),
    group_label = factor(group_label)
  )

# VARIABLE SELECTION ———————————————————————————————————————————————————————————
# remove factors and identifiers
predictors <- master %>%
  select(-LAT, -LON, 
         -biome, 
         -ownership, 
         -status, 
         -group_label, 
         -managed, 
         -ll_id, 
         -PID)

# cluster the variables
clusts <- hclustvar(predictors)

plot(clusts)

# run stability analysis
stab <- stability(clusts)

plot(stab, main = "Stability of partitions")

# convert factors to dummy variables for correlation analysis
predictors <- model.matrix(~ . - 1, data = predictors)

# calculate correlation matrix
corrs <- cor(predictors, use = "pairwise.complete.obs")

# visualise correlations
corrplot(corrs, method = "circle", type = "lower", 
         tl.pos = "n", tl.col = "black",
         diag = FALSE, cl.ratio = 0.2)

# find highly correlated variables 
highCorr <- findCorrelation(corrs, cutoff = 0.7)
print(colnames(predictors)[highCorr])

rm(highCorr, predictors, stab, clusts)

# ——————————————————————————————————————————————————————————————————————————————

# subset selected variables
dataMaster <- master %>%
  select( PID, LAT, LON,
    # stability
    meanEVI, sdResidEVI, coeffVarEVI,
    
    # functional diversity indices
    FDis, FDiv, FEve, RaoQ,
    
    # diversity indices
    speciesRichness, shannonDiversity,
    
    # categorical FIA vars
    biome, ownership, managed, group_label,
    
    # temperature
    CHELSA_BIO_Annual_Mean_Temperature, 
    CHELSA_BIO_Temperature_Annual_Range,
    
    # precipitation
    CHELSA_BIO_Annual_Precipitation, 
    CGIAR_Aridity_Index,
    
    # soil
    SG_Soil_pH_H2O_015cm, 
    SG_SOC_Content_015cm, 
    SG_Clay_Content_015cm,
    
    # topography
    EarthEnvTopoMed_Elevation, 
    EarthEnvTopoMed_Slope,
    SG_Depth_to_bedrock,
    
    # environmental stress 
    EarthEnvCloudCover_intraAnnualSD, 
    WorldClim2_WindSpeed_AnnualMean, 
    CrowtherLab_SoilMoisture_intraAnnualSD_downsampled10km,
    
    # disturbance 
    GFAD_FractionOfRegrowthForest_downsampled50km, 
    GFAD_regrowthForestAge_Mean_downsampled50km,
    EsaCci_BurntAreasProbability, 
    CrowtherLab_RootMassFraction,
    
    # human influence 
    WCS_Human_Footprint_2009,
    CrowtherLab_IntactLandscapes,
    GranthamEtAl_ForestLandscapeIntegrityIndex,
    CHELSA_exBIO_GrowingSeasonLength,
    
  )
# clean up variable names
dataMaster <- dataMaster %>%
  rename(
    forestType = group_label,
    tempAnnual = CHELSA_BIO_Annual_Mean_Temperature,
    tempRange = CHELSA_BIO_Temperature_Annual_Range,
    precipitation = CHELSA_BIO_Annual_Precipitation,
    aridity = CGIAR_Aridity_Index,
    soilpH = SG_Soil_pH_H2O_015cm,
    soilSOC = SG_SOC_Content_015cm,
    soilClay = SG_Clay_Content_015cm,
    bedrockDepth = SG_Depth_to_bedrock,
    forestAge = GFAD_regrowthForestAge_Mean_downsampled50km,
    topoSlope = EarthEnvTopoMed_Slope,
    topoElevation = EarthEnvTopoMed_Elevation,
    cloudCover = EarthEnvCloudCover_intraAnnualSD,
    windSpeed = WorldClim2_WindSpeed_AnnualMean,
    soilMoisture = CrowtherLab_SoilMoisture_intraAnnualSD_downsampled10km,
    forestRegrowth = GFAD_FractionOfRegrowthForest_downsampled50km,
    burntAreas = EsaCci_BurntAreasProbability,
    rootMass = CrowtherLab_RootMassFraction,
    humanFootprint = WCS_Human_Footprint_2009,
    forestIntegrity = GranthamEtAl_ForestLandscapeIntegrityIndex,
    intactLandscapes = CrowtherLab_IntactLandscapes,
    growingSeason = CHELSA_exBIO_GrowingSeasonLength
  )

# scale environmental variables
dataMaster <- dataMaster %>%
  mutate(
    tempAnnual = tempAnnual / 10, 
    tempRange = tempRange / 10,
    soilpH = soilpH / 10
  )
  
biomeCounts <- dataMaster %>%
  count(biome)


write.csv(dataMaster, "master.csv")

# spatial autocorrelation
spatial <- SpatialPointsDataFrame(coords = cbind(dataMaster$LON,
                                                 dataMaster$LAT), 
                                  data = dataMaster)
