#' Quantify ATP production rate
#' 
#' This function quantifies glycoAPR and mitoAPR based on OCR and ECAR
#' 
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_assay a \code{character} which can be "XFp", "XFe24", "XF" or "XFe96"
#' @param which_level a character either "well" or "group"
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @export
summarize_apr <- function(df, which_assay, which_level) {
  representive_ocr <- summarize_ocr(df, which_level = "well")
  
  # glycoAPR quantification
  glycoAPR_dat <- representive_ocr %>% 
    select(Well, Group, init_ocr, rot_ocr)
  
  ECAR_dat <- df %>% 
    select(Measurement, Well, Group, ECAR)
  
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
  
  if (which_level == "well") {
    return(APR_dat)
  } else if (which_level == "group") {
    APR_dat %>% 
      gather(what, val, -Well, -Group) %>% 
      group_by(Group, what) %>% 
      summarize(mean_val = mean(val)) %>% 
      ungroup() %>% 
      spread(what, mean_val)
  }
}
