library(data.table)

processGEE <- function(file_path) {
  # read the data from the specified file path
  GEEdata <- fread(file_path)
  
  # clean up column names by trimming whitespace
  names(GEEdata) <- trimws(names(GEEdata))
  
  # select and return only the relevant columns: PID, mean, and year
  GEEdata <- GEEdata[, .(PID, mean, year)]
  
  return(GEEdata)
}
