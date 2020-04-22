#' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
import_seahorse <- function(x) {
  read_excel(x, sheet = "Rate")
}