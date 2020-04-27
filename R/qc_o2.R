#' O2 quality control
#' 
#' This function flags wells with problematic O2 levels
#' @param df a \code{data.frame} with raw data and produced by \code{import_seahorse} 
#' @param o2.threshold a numeric value that an O2 level cannot be higher than
#' @import dplyr
#' @export
qc_o2 <- function(df, o2.threshold = 160) {
  df %>% 
    dplyr::select(Measurement, Well, `O2 (mmHg)`) %>% 
    dplyr::mutate(o2_fail = ifelse(`O2 (mmHg)` > o2.threshold, TRUE, FALSE))
} 
