#' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @return a list with two \code{data.frame} corresponding to the Rate and Raw data
#' @import readxl
#' @export
import_seahorse <- function(x) {
  dat_rate <- read_excel(x, sheet = "Rate")
  dat_raw <- read_excel(x, sheet = "Raw")
  dat_log <- read_excel(x, sheet = "Operation Log")
  
  dat_seahorse <- list()
  dat_seahorse$Rate <- dat_rate
  dat_seahorse$Raw <- dat_raw
  dat_seahorse$Log <- dat_log
  
  return(dat_seahorse)
}
