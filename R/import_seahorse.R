#' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
#' @example #' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
#' @example 
#' import_seahorse("seahorse_c2c12.xlsx")
#' 
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'   \item Measurement 
#'   \item Well 
#'   \item Group 
#'   \item Time 
#'   \item OCR 
#'   \item ECAR 
#'   \item PER 
#' }
#' #' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
#' @example 
#' import_seahorse("seahorse_c2c12.xlsx")
#' 
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'   \item Measurement 
#'   \item Well 
#'   \item Group 
#'   \item Time 
#'   \item OCR 
#'   \item ECAR 
#'   \item PER 
#' }
#' #' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
#' @example 
#' import_seahorse("seahorse_c2c12.xlsx")
#' 
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'   \item Measurement 
#'   \item Well 
#'   \item Group 
#'   \item Time 
#'   \item OCR 
#'   \item ECAR 
#'   \item PER 
#' }
#' #' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
#' @example 
#' import_seahorse("seahorse_c2c12.xlsx")
#' 
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'   \item Measurement 
#'   \item Well 
#'   \item Group 
#'   \item Time 
#'   \item OCR 
#'   \item ECAR 
#'   \item PER 
#' }
#' #' Import Seahorse data
#' 
#' Load Seahorse Excel files into R as a data frame
#' 
#' Import the Excel spreadsheet which contains the Seahorse XF assay result
#' @param x Excel spreadsheet which contains the Seahorse XF assay result
#' @import readxl
#' @export
#' @example 
#' import_seahorse("seahorse_c2c12.xlsx")
#' 
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'   \item Measurement 
#'   \item Well 
#'   \item Group 
#'   \item Time 
#'   \item OCR 
#'   \item ECAR 
#'   \item PER 
#' }
#' 
#' import_seahorse("seahorse_c2c12.xlsx")
#' 
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'   \item Measurement 
#'   \item Well 
#'   \item Group 
#'   \item Time 
#'   \item OCR 
#'   \item ECAR 
#'   \item PER 
#' }
#' 

import_seahorse <- function(x) {
  read_excel(x, sheet = "Rate")
}