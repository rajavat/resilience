library(dplyr)

library(ranger)
library(fastshap)
library(shapviz)

# load in data
dataMaster <- read.csv("data/master.csv")

# number of threads for parallelisation 
nthreads <- 12

# set the seed
set.seed(123)

# ——————————————————————————————————————————————————————————————————————————————

# 1. full model
lmFull <- lm(sdResidEVI ~ meanEVI + (FDis + FDiv + FEve) + (
  speciesRichness + # species diversity
    tempAnnual + # temperature
    precipitation + aridity + # precipitation
    soilpH + soilSOC + soilClay + # soil
    topoElevation + topoSlope + bedrockDepth + # topography
    cloudCover + windSpeed + soilMoisture + # environ stress
    forestRegrowth + forestAge + burntAreas + rootMass + # disturbance
    humanFootprint + forestIntegrity + # human influence
    intactLandscapes + growingSeason) 
  + biome + managed, 
  data = dataMaster)

# ——————————————————————————————————————————————————————————————————————————————
# count entries per biome
biomeCounts <- dataMaster %>%
  count(biome)

# function to run models for a specific biome
modelBiome <- function(b, data) {
  # filter data for the specific biome
  biomeData <- data %>% filter(biome == b)
  
  # Split into training and testing sets (80/20)
  set.seed(123)
  train_indices <- sample(1:nrow(biomeData), 0.8 * nrow(biomeData))
  train <- biomeData[train_indices, ]
  test <- biomeData[-train_indices, ]
  
  # Print information about the biome dataset
  cat("\n\n======================================================\n")
  cat("MODELS FOR BIOME:", b, "\n")
  cat("Number of observations:", nrow(biomeData), "\n")
  cat("======================================================\n\n")
  
  # run linear model
  lmBiome <- lm(sdResidEVI ~ meanEVI + (FDis + FDiv + FEve) +
           (speciesRichness + # species diversity
            tempAnnual + # temperature
            precipitation + aridity + # precipitation
            soilpH + soilSOC + soilClay + # soil
            topoElevation + topoSlope + bedrockDepth + # topography
            cloudCover + windSpeed + soilMoisture + # environ stress
            forestRegrowth + forestAge + burntAreas + rootMass + # disturbance
            humanFootprint + forestIntegrity + # human influence
            intactLandscapes + growingSeason) 
           + managed, 
          data = train)
  
  # print linear model summary
  cat("LINEAR MODEL SUMMARY:\n")
  print(summary(lmBiome))
  
  # run ranger model
  rfBiome <- ranger(
    formula = sdResidEVI ~ meanEVI + (FDis + FDiv + FEve) + 
      (speciesRichness +
         tempAnnual +
         precipitation + aridity +
         soilpH + soilSOC + soilClay +
         topoElevation + topoSlope + bedrockDepth +
         cloudCover + windSpeed + soilMoisture +
         forestRegrowth + forestAge + burntAreas + rootMass +
         humanFootprint + forestIntegrity +
         intactLandscapes + growingSeason) + managed,
    data = train,
    importance = "permutation",
    write.forest = TRUE,
    num.threads = nthreads,
    seed = 123
  )
  
  # print ranger model results
  cat("\nRANGER MODEL RESULTS:\n")
  print(rfBiome)
  
  # print variable importance
  cat("\nVARIABLE IMPORTANCE:\n")
  print(importance(rfBiome))
  
  # calculate SHAP values if the dataset is large enough
  if(nrow(train) > 30) {
    cat("\nCalculating SHAP values...\n")
    
    # create predictor matrix
    X <- train %>% select(-sdResidEVI)
    
    # define prediction wrapper
    predwrapper <- function(model, newdata) {
      predict(model, data = newdata)$predictions
    }
    
    # calculate SHAP values
    shap <- fastshap::explain(
      rfBiome,
      pred_wrapper = predwrapper,
      X = X,
      nsim = 100
    )
    
    # create shapviz object
    sv <- shapviz::shapviz(
      shap,
      X = X,
      baseline = mean(train$sdResidEVI)
    )
    
    # return both models and SHAP values
    return(list(
      linear_model = lmBiome,
      ranger_model = rfBiome,
      shap_values = sv,
      train_data = train,
      test_data = test
    ))
  } else {
    # return just the models if dataset is too small for SHAP
    return(list(
      linear_model = lmBiome,
      ranger_model = rfBiome,
      train_data = train,
      test_data = test
    ))
  }
}

# run models for each biome 
biomeModels <- list()
for(b in biomeCounts$biome) {
  biomeModels[[b]] <- modelBiome(toString(b), dataMaster)
}

# print summary of all biome models
cat("\n\n======================================================\n")
cat("SUMMARY OF ALL BIOME MODELS\n")
cat("======================================================\n\n")

for(b in names(biomeModels)) {
  cat("Biome:", b, "\n")
  cat("Linear model R-squared:", summary(biomeModels[[b]]$linearModel)$r.squared, "\n")
  cat("Ranger model R-squared:", biomeModels[[b]]$rangerModel$r.squared, "\n")
}
