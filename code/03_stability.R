# INITIALISE ———————————————————————————————————————————————————————————————————
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(broom)
library(vegan)


# cleanGEE ——————————————————————————————————————————————————————————————————————
# function to process and clean earth engine data
# input: path to data file
# output: cleaned data in df 

cleanGEE <- function(file_path) {
  # read the data from the specified file path
  GEEdata <- fread(file_path)
  
  # clean up column names by trimming whitespace
  names(GEEdata) <- trimws(names(GEEdata))
  
  GEEdata <- GEEdata %>%
    mutate(
      lon = as.numeric(str_extract(.geo, "(?<=\\[)[-0-9.]+")),
      lat = as.numeric(str_extract(.geo, "(?<=,)[-0-9.]+"))
    )
  
  # select and return only the relevant columns: PID, mean, and year
  GEEdata <- GEEdata[, .(PID, mean, year, lat, lon)]
  
  return(GEEdata)
}

dataEVI <- cleanGEE("data/GEE/raw/MODIS_EVI_QA_1744100860265.csv")
# write.csv(EVIdata, "EVIdataQA.csv", row.names = FALSE)

# PROCESS EVI ——————————————————————————————————————————————————————————————————

dataEVI <- read.csv("data/GEE/EVIdataQA.csv") %>% 
  drop_na()

# perform linear regression for each PID
lmEVI <- dataEVI %>%
  group_by(PID) %>%
  do({model = lm(mean ~ year, data = .)
  data.frame(
    PID = .$PID[1],
    residual = resid(model),
    mean = mean(.$mean)
  )
  })

# calculate the variance in residuals for each PID
stability <- lmEVI %>%
  group_by(PID) %>%
  summarise(
    meanEVI = mean(mean),
    sdResidEVI = sd(residual),
    coeffVarEVI = sd(residual) / mean(mean),
  )

# write.csv(stability, "stability.csv")

# RICHNESS  ————————————————————————————————————————————————————————————————————
# calculates species richness and shannons index for each PID

dataFIA <- read_feather("data/FIA/FIAmaster.arrow")

diversity <- dataFIA %>%
  group_by(PID) %>%
  summarize(
    speciesRichness = n_distinct(accepted_bin),
    .groups = "drop"
  ) %>%
  left_join(dataFIA %>%
              count(PID, species) %>%
              group_by(PID) %>%
              summarize(
                shannonDiversity = diversity(n, index = "shannon"),
                .groups = "drop"
              ),
            by = "PID"
  )

# write.csv(diversity, "diversityMetrics.csv")