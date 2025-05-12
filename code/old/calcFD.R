# INITIALISE ———————————————————————————————————————————————————————————————————
library(tidyverse) 

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
options(future.globals.maxSize = 2048^3)


# LOAD DATA ————————————————————————————————————————————————————————————————————
# load abundance and trait data

# create aligned abundance and trait matrices
FDm <- createMatrices(abundanceData, traitMatrix, "baPa")
# baPa - basal area
# wcPa - weighted carbon per acre
# bpa - biomass per acre
# tpa - trees per acre

# PREP DATA ————————————————————————————————————————————————————————————————————

# check for NAs
anyNA(FDm$abundMatrix)
anyNA(FDm$traitMatrix)

FDm$abundMatrix[is.na(FDm$abundMatrix)] <- 0

# replace spaces in species and trait names with underscores jic
rownames(FDm$traitMatrix) <- gsub(" ", "_", rownames(FDm$traitMatrix))
colnames(FDm$traitMatrix) <- gsub(" ", "_", colnames(FDm$traitMatrix))
colnames(FDm$abundMatrix) <- gsub(" ", "_", colnames(FDm$abundMatrix))

# check that species names match between matrices
all(colnames(FDm$abundMatrix) %in% rownames(FDm$traitMatrix))

# subset traits
FDm$traitMatrix <- FDm$traitMatrix %>%
  select(Bark_thickness, Wood_density,
         Root_depth, Stomatal_conduct.,
         Specific_leaf_area, Conduit_diam.,
         Leaf_density)

# log transform traits 
FDm$traitMatrix <- FDm$traitMatrix %>%
  mutate(across(everything(), ~log(.x + 1))) 

# standardise traits 
FDm$traitMatrix <- as.data.frame(scale(FDm$traitMatrix))

# CALCULATE INDICES ————————————————————————————————————————————————————————————

# 1. dbFD method ———————————————————————————————————————————————————————————————
FDindices_dbFD <- dbFD(
  x = FDm$traitMatrix,       
  a = FDm$abundMatrix,
  corr = "lingoes",       # correction method for negative eigenvalues
  calc.FRic = TRUE,       # calculate Functional Richness
  calc.FDiv = TRUE,       # calculate Functional Divergence
  calc.CWM = TRUE,        # calculate Community-Weighted Means
  calc.FGR = FALSE        # calculate Functional Group Richness
)


# create dataframe with dbFD indices
FDf_FD <- tibble(
  PID = rownames(FDm$abundMatrix)) %>%
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
FDm$abundMatrix <- as.matrix(FDm$abundMatrix)
FDm$traitMatrix <- as.matrix(FDm$traitMatrix)

future::plan(future::multisession)
FDindices_fundiv <- list(
  FDis = fd_fdis(traits = FDm$traitMatrix, sp_com = FDm$abundMatrix),
  FDiv = fd_fdiv(traits = FDm$traitMatrix, sp_com = FDm$abundMatrix),
  FEve = fd_feve(traits = FDm$traitMatrix, sp_com = FDm$abundMatrix),
  FRic = fd_fric(traits = FDm$traitMatrix, sp_com = FDm$abundMatrix),
  RaoQ = fd_raoq(traits = FDm$traitMatrix, sp_com = FDm$abundMatrix)
)

# create df with fundiversity output
FDf_fundiv <- FDindices$FDis %>%
  full_join(FDindices$FDiv, by = "site") %>%
  full_join(FDindices$FEve, by = "site") %>%
  full_join(FDindices$FRic, by = "site") %>%
  full_join(FDindices$RaoQ, by = "site") %>%
  na.omit()

# write.csv(FDf, "FD.csv", row.names = TRUE)