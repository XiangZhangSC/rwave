#' Representive OCR values in mito stress test
#' 
#' This function returns a set of OCR values describing mito stress test
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_level a character "well" or "group"
#' @import dplyr
#' @import tidyr
#' @export
summarize_ocr <- function(df, which_level = "well") {
  df_summary_well <- df %>% 
    select(Measurement, Well, Group, OCR) %>% 
    spread(Measurement, OCR) %>% 
    rename(init_ocr = `3`, 
           oligo_ocr = `6`, 
           fccp_ocr = `7`, 
           rot_ocr = `12`) %>% 
    select(Well, Group, init_ocr, oligo_ocr, fccp_ocr, rot_ocr) %>% 
    mutate(non_mito_ocr = rot_ocr, 
           basal_ocr = init_ocr - non_mito_ocr, 
           max_ocr = fccp_ocr - rot_ocr, 
           spare_ocr = max_ocr - basal_ocr, 
           atp_ocr = basal_ocr - oligo_ocr, 
           proton_leak = oligo_ocr - non_mito_ocr, 
           relative_spare_capacity = spare_ocr / basal_ocr, 
           coupling_efficiency = atp_ocr / basal_ocr, 
           relative_proton_leak = 1 - coupling_efficiency)
  
  if (which_level == "group") {
    df_summary_group <- df_summary_well %>% 
      gather(what, val, -Well, -Group) %>% 
      group_by(Group, what) %>% 
      summarize(mean_val = mean(val)) %>% 
      spread(what, mean_val) %>% 
      ungroup()
    
    return(df_summary_group)
  } else if (which_level == "well") {
    return(df_summary_well)
  }
}
