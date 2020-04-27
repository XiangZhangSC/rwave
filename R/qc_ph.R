#' pH quality control
#' 
#' This function flags wells with problematic pH
#' @param df a \code{data.frame} with raw data and produced by \code{import_seahorse} 
#' @param pH.threshold a numeric value a pH value cannot be lower than
#' @import dplyr
#' @export
qc_ph <- function(df, pH.threshold = 7.0) {
  df %>% 
    dplyr::select(Measurement, Well, pH) %>% 
    dplyr::mutate(pH_fail = ifelse(pH < pH.threshold, TRUE, FALSE))
} 
