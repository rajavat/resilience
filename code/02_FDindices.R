# INITIALISE ———————————————————————————————————————————————————————————————————
library(tidyverse) 
library(arrow)

# parallelisation
library(furrr)
library(future)

# plotting tools
library(ggplot2)
library(viridis)
library(patchwork)

# computing indices 
library(FD)
library(fundiversity)
library(hypervolume)

# setting maxSize for parallelisation
plan(multisession, workers = 20)
options(future.globals.maxSize = 2000 * 1024^2)

# TRAIT MATRIX —————————————————————————————————————————————————————————————————
# generates a matrix of trait data given a trait table as input
dataTRY <- read.csv("data/trait/raw/TRY_data.csv")

traitMatrix <- dataTRY %>%
  arrange(accepted_bin, trait_short, row_number()) %>%
  select(accepted_bin, trait_short, pred_value) %>% 
  pivot_wider(
    names_from = trait_short,
    values_from = pred_value,
    values_fn = list(pred_value = last), 
    values_fill = NA
  ) %>%
  column_to_rownames("accepted_bin")
# write.csv(traitMatrix, "traitMatrix.csv", row.names = TRUE)

# ABUNDANCE MATRIX —————————————————————————————————————————————————————————————

dataFIA <- read_feather("data/FIA/FIAmaster.arrow")

# filter out grasslands and merge forest categories
dataFIA <- dataFIA %>%
  # remove all grassland types
  filter(!biome %in% c("Flooded grasslands",
                       "Temperate grasslands",
                       "Tropical grasslands")) %>%
  # create new merged biome categories
  mutate(biome = case_when(
    # merge all tropical forests
    biome %in% c("Tropical coniferous forests", 
                 "Tropical dry broadleaf forests",
                 "Tropical moist broadleaf forests") ~ "Tropical forests",
    # merge all temperate forests
    biome %in% c("Temperate broadleaf forests",
                 "Temperate conifer forests") ~ "Temperate forests",
    # keep other biomes as they are
    TRUE ~ biome
  ))

# count entries per biome
biomeCounts <- dataFIA %>%
  count(biome)

# create a table with abundance data for each plot for each metric
# baPa - basal area - total cross sectional area of metabolically active matter
# wcPa - weighted carbon per acre
# bpa - biomass per acre
# tpa - trees per acre
abundance <- dataFIA %>% 
  mutate(
    baPa = pi * (DIA / 2)^2 * TPA_UNADJ,
    wcPa = CARBON_AG * TPA_UNADJ
  )
abundance <- abundance %>% 
  group_by(PID, accepted_bin) %>%
  summarise(
    baPa = sum(baPa),
    wcPa = sum(wcPa),
    bpa = sum(BPA),
    tpa = sum(TPA_UNADJ),
    .groups = "drop"
  )

# write.csv(abundanceData, "abundance.csv", row.names = TRUE)

# abundanceData <- read.csv("data/FIA/abundance.csv")
# traitMatrix <- read.csv("data/trait/traitMatrix.csv")

# CREATE MATRICES ——————————————————————————————————————————————————————————————
# function to create aligned matrices for each metric, input abundance data,
# trait matrix, and the required abundance metric

createMatrices <- function(abunData, traitMatrix, metricName) {
  # create abundance matrix
  abundanceMatrix <- abunData %>%
    pivot_wider(
      id_cols = PID,
      names_from = accepted_bin,
      values_from = all_of(metricName),
      values_fill = 0
    ) %>%
    column_to_rownames("PID")
  
  # find common species
  commonSpecies <- intersect(colnames(abundanceMatrix), rownames(traitMatrix))
  
  # check if we have common species
  if(length(commonSpecies) == 0) {
    return(list(abundMatrix = NULL, traitMatrix = NULL))
  }
  
  # align matrices
  alignedAbundMatrix <- abundanceMatrix[, commonSpecies, drop = FALSE]
  
  # identify and remove plots with zero abundance
  nonZeroPlots <- rowSums(alignedAbundMatrix, na.rm = TRUE) > 0
  
  # check if we have any non-zero plots 
  if(!any(nonZeroPlots, na.rm = TRUE)) {
    return(list(abundMatrix = NULL, traitMatrix = NULL))
  }
  
  # filter to non-zero plots
  finalAbundMatrix <- alignedAbundMatrix[nonZeroPlots, , drop = FALSE]
  
  
  # return aligned matrices
  list(
    abundMatrix = finalAbundMatrix,
    traitMatrix = traitMatrix[commonSpecies, , drop = FALSE]
  )
}

# LOAD DATA ————————————————————————————————————————————————————————————————————
# load abundance and trait data

# create aligned abundance and trait matrices
matrices <- createMatrices(abundance, traitMatrix, "baPa")
# baPa - basal area
# wcPa - weighted carbon per acre
# bpa - biomass per acre
# tpa - trees per acre

rm(traitMatrix)
rm(abundance)

# PREP DATA ————————————————————————————————————————————————————————————————————

# check for NAs
anyNA(matrices$abundMatrix)
anyNA(matrices$traitMatrix)

matrices$abundMatrix[is.na(matrices$abundMatrix)] <- 0

# replace spaces in species and trait names with underscores jic
rownames(matrices$traitMatrix) <- gsub(" ", "_", rownames(matrices$traitMatrix))
colnames(matrices$traitMatrix) <- gsub(" ", "_", colnames(matrices$traitMatrix))
colnames(matrices$abundMatrix) <- gsub(" ", "_", colnames(matrices$abundMatrix))

# check that species names match between matrices
all(colnames(matrices$abundMatrix) %in% rownames(matrices$traitMatrix))

# subset traits
matrices$traitMatrix <- matrices$traitMatrix %>%
  select(Leaf_density, Root_depth, 
         Specific_leaf_area, 
         Stomatal_conduct., Leaf_K, 
         Conduit_diam., Leaf_area,
         Crown_height, Seed_dry_mass,
         Bark_thickness)

# log transform traits 
matrices$traitMatrix <- matrices$traitMatrix %>%
  mutate(across(everything(), ~log(.x + 1))) 

# standardise traits 
matrices$traitMatrix <- as.data.frame(scale(matrices$traitMatrix))

# CALCULATE INDICES ————————————————————————————————————————————————————————————

# 1. dbFD method ———————————————————————————————————————————————————————————————
FDindices_dbFD <- dbFD(
  x = matrices$traitMatrix,       
  a = matrices$abundMatrix,
  corr = "lingoes",       # correction method for negative eigenvalues
  calc.FRic = TRUE,       # calculate Functional Richness
  calc.FDiv = TRUE,       # calculate Functional Divergence
  calc.CWM = TRUE,        # calculate Community-Weighted Means
  calc.FGR = FALSE        # calculate Functional Group Richness
)


# create dataframe with dbFD indices
FDf_FD <- tibble(
  PID = rownames(matrices$abundMatrix)) %>%
  mutate( 
    FRic = FDindices_dbFD$FRic,
    FEve = FDindices_dbFD$FEve,
    FDiv = FDindices_dbFD$FDiv,
    FDis = FDindices_dbFD$FDis,
    RaoQ = FDindices_dbFD$RaoQ) %>%
  left_join( # join w CWM data
    tibble(
      PID = rownames(FDindices_dbFD$CWM),
      as_tibble(FDindices_dbFD$CWM)), 
    by = "PID") %>%
  drop_na()

# write.csv(FDf, "FD.csv", row.names = TRUE)

# 2. fundiversity ——————————————————————————————————————————————————————————————

# convert data to matrices for computation
matrices$abundMatrix <- as.matrix(matrices$abundMatrix)
matrices$traitMatrix <- as.matrix(matrices$traitMatrix)

plan(multisession, workers = 20)
FDindices_fundiv <- list(
  FDis = fd_fdis(traits = matrices$traitMatrix, sp_com = matrices$abundMatrix),
  FDiv = fd_fdiv(traits = matrices$traitMatrix, sp_com = matrices$abundMatrix),
  FEve = fd_feve(traits = matrices$traitMatrix, sp_com = matrices$abundMatrix),
  FRic = fd_fric(traits = matrices$traitMatrix, sp_com = matrices$abundMatrix),
  RaoQ = fd_raoq(traits = matrices$traitMatrix, sp_com = matrices$abundMatrix)
)

# create df with fundiversity output
FDf_fundiv <- FDindices_fundiv$FDis %>%
  full_join(FDindices_fundiv$FDiv, by = "site") %>%
  full_join(FDindices_fundiv$FEve, by = "site") %>%
  full_join(FDindices_fundiv$FRic, by = "site") %>%
  full_join(FDindices_fundiv$RaoQ, by = "site") %>%
  na.omit()

FDf_fundiv <- dataFD %>% 
  rename(
    PID = site,
    RaoQ = Q
  )

write.csv(FDf_fundiv, "FD.csv", row.names = TRUE)