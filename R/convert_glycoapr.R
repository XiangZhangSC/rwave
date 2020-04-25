#' Quantify glycoAPR
#' 
#' This function calculates glycolytic ATP production rate based on ECAR
#' 
#' @param x a numeric value or a vector of ECAR
#' @param init_ocr a numeric value of initial OCR
#' @param rot_ocr a numeric value of OCR AA/rot
#' @param which_assay a \code{character} which can be "XFp", "XFe24", "XF" or "XFe96"
#' 
#' @export
convert_glycoapr <- function(x, init_ocr, rot_ocr, which_assay) {
  
  ECAR <- x
  
  BF <- 2.40
  Vol <-  2.28
  
  if (which_assay == "XFp") {
    CCF <- 0.50
    Kvol = 1.1
  } else if (which_assay == "XFe24") {
    CCF <- 0.60
    Kvol = 1.1
  } else if (which_assay %in% c("XF", "XFe96")) {
    CCF <- 0.61
    Kvol = 1.6
  }
  
  mitoOCR <- init_ocr - rot_ocr
  mitoPER <- mitoOCR * CCF
  PER <- ECAR * BF * Vol * Kvol
  glycoAPR <- PER - mitoPER
  
  return(glycoAPR)
}
