#' Quantify mitoAPR
#' 
#' This function calculates mitochondrial ATP production rate based on OCR
#' 
#' @param x the rate of oxygen consumption that is coupled to ATP production during OXPHOS
#' 
#' @export
convert_mitoapr <- function(x) {
  
  OCR <- x # OCR can be atp_ocr or max_ocr
  
  po <- 2.75
  
  mitoAPR <- OCR * 2 * po
  
  return(mitoAPR)
}