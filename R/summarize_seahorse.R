#' Summarize Sehorse in few parameters
#'
#' summarize_seahorse summarizes Seahorse analysis in few parameters
#' @param df a data frame produced by \code{import_seahorse}
#' @param which_assay a character which can be "XFp", "XFe24", "XF" or "XFe96"
#' @import dplyr
#' @export

summarize_seahorse <- function(df, which_assay) {
  proton_leak <- summarize_ocr(df, which_level = "group") %>% 
    select(Group, proton_leak)
  
  APR <- summarize_apr(df, which_assay = which_assay, which_level = "group") %>% 
    select(Group, basal_glycoAPR, max_glycoAPR, basal_mitoAPR, max_mitoAPR)
  
  proton_leak %>% 
    inner_join(APR, by = "Group") %>% 
    rename(`Max GlycoAPR` = max_glycoAPR, 
           `Basal GlycoAPR` = basal_glycoAPR, 
           `Max mitoAPR` = max_mitoAPR, 
           `Basal mitoAPR` = basal_mitoAPR, 
           `Proton leak` = proton_leak)
}
