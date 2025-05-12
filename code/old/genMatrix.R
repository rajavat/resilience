# INITIALISE ———————————————————————————————————————————————————————————————————
library(tidyr)
library(dplyr)
library(tidyverse)


# TRAIT MATRIX —————————————————————————————————————————————————————————————————
# generates a matrix of trait data given a trait table as input
traitData <- read.csv("data/trait/raw/TRY_data.csv")

traitMatrix <- traitData %>%
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

FIAmaster <- read_feather("data/FIAmaster.arrow")

# create a table with abundance data for each plot for each metric
# baPa - basal area - total cross sectional area of metabolically active matter
# wcPa - weighted carbon per acre
# bpa - biomass per acre
# tpa - trees per acre
abundanceData <- FIAmaster %>% 
  mutate(
    baPa = pi * (DIA / 2)^2 * TPA_UNADJ,
    wcPa = CARBON_AG * TPA_UNADJ
  )
abundanceData <- abundanceData %>% 
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

# ——————————————————————————————————————————————————————————————————————————————
