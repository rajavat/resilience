library(tidyverse)
library(arrow)
library(furrr)
library(future)

# loadFIA ——————————————————————————————————————————————————————————————————————
# function to read in all FIA state arrow files in parallel
# input: path to FIA state files 
# output: dataframe titled FIAmaster with all state data merged

loadFIA <- function(fia_data_path = "data/FIA/states/") {
  stateCodes <- c("ma", "me", "vt", "ct", "nh", "ri", 
                  "ak", "al", "ar", "as", "az", "ca",
                  "co", "de", "fl", "fm", "ga", "gu", 
                  "hi", "ia", "id", "il", "in", "ks",
                  "ky", "la", "md", "mh", "mi", "mn", 
                  "mo", "mp", "ms", "mt", "nc", "nd",
                  "ne", "nj", "nm", "nv", "ny", "oh", 
                  "ok", "or", "pa", "pr", "pw", "sc",
                  "sd", "tn", "tx", "ut", "va", "vi", 
                  "wa", "wi", "wv", "wy")
  
  fiaCodes <- read_csv("data/FIA/raw/meta/FIA_forest_types.csv") %>%
    select(VALUE, group_label, TYPGRPCD, MEANING)

  processState <- function(stateCode) {  
    minDia <- 5
    filePath <- file.path(fia_data_path, paste0("FIA_", 
                                                toupper(stateCode), ".arrow"))
    
    df <- read_feather(filePath)
    
    filtered_df <- df %>%
      filter(alive == 1, forested == 1, cdMult == 0, 
             lastYear == TRUE, DIA >= minDia)
    
    if (stateCode %in% c("ak", "ca", "id", "or", "wa")) {
      filtered_df <- filtered_df %>%
        filter(goodDesign == 1 | DESIGNCD %in% c(501:506))
    } else {
      filtered_df <- filtered_df %>%
        filter(goodDesign == 1)
    }
    
    filtered_df %>%
      left_join(fiaCodes, by = c("FORTYPCD" = "VALUE")) %>%
      rename(FIA_label_detail = MEANING) %>%
      mutate(stateCode = stateCode)
  }
  
  plan(multisession)
  FIAmaster <- stateCodes %>%
    future_map_dfr(processState, .options = furrr_options(seed = TRUE)) %>%
    filter(!is.na(group_label), group_label != 'Nonstocked')
  
  return(FIAmaster)
}

# ——————————————————————————————————————————————————————————————————————————————

FIAmaster <- loadFIA()