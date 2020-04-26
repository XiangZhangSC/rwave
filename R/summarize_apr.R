#' Quantify ATP production rate
#' 
#' This function quantifies glycoAPR and mitoAPR based on OCR and ECAR
#' 
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_assay a \code{character} which can be "XFp", "XFe24", "XF" or "XFe96"
#' 
#' @import purrr
#' @import dplyr
#' 
#' @export
summarize_apr <- function(df, which_assay) {
  representive_ocr <- summarize_ocr(df)
  
  # glycoAPR quantification
  glycoAPR_dat <- representive_ocr %>% 
    dplyr::select(Well, Group, init_ocr, rot_ocr)
  
  ECAR_dat <- df %>% 
    dplyr::select(Measurement, Well, Group, ECAR)
  
  updated_glycoAPR_dat <- glycoAPR_dat %>% 
    left_join(ECAR_dat, by = c("Well", "Group")) %>% 
    mutate(glycoAPR = pmap_dbl(list(ECAR, init_ocr, rot_ocr), convert_glycoapr, which_assay = which_assay))
  
  final_glycoAPR_dat <- updated_glycoAPR_dat %>% 
    filter(Measurement == 3 | Measurement == 6) %>% 
    dplyr::select(Well, Group, Measurement, glycoAPR) %>% 
    spread(Measurement, glycoAPR) %>% 
    rename(basal_glycoAPR = `3`, 
           max_glycoAPR = `6`)
  
  # mitoAPR quantification
  mitoAPR_dat <- representive_ocr %>% 
    dplyr::select(Well, Group, atp_ocr, max_ocr) %>% 
    mutate(mitoAPR = map_dbl(atp_ocr, convert_mitoapr), 
           max_mitoAPR = map_dbl(max_ocr, convert_mitoapr))
  
  final_mitoAPR_dat <- mitoAPR_dat %>% 
    dplyr::select(Well, Group, mitoAPR, max_mitoAPR) %>% 
    dplyr::rename(basal_mitoAPR = mitoAPR)
  
  APR_dat <- final_mitoAPR_dat %>% 
    left_join(final_glycoAPR_dat, by = c("Well", "Group")) %>% 
    mutate(total_space = max_mitoAPR * max_glycoAPR, 
           spare_mitoAPR = max_mitoAPR - basal_mitoAPR, 
           spare_glycoAPR = max_glycoAPR - basal_glycoAPR)
  
  return(APR_dat)
}