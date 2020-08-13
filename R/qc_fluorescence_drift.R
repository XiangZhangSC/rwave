#' Quality control of fluorescence drift
#'
#' Estimation of drift constants in temperature control wells
#'
#' @param dat.raw a data frame with raw data and produced by \code{import_seahorse} 
#' @import astsa
#' @import dplyr
#' @import tidyr
#' @export

qc_fluorescence_drift <- function(dat) {
    
    add_drift_constant <- function(df) {
        fluorescence <- ts(df$`O2 Corrected Em.`, start = 0)
        
        x <- log(fluorescence)
        # fit a Random Walk model with drift
        rw_drift_mod <- sarima(x, p = 0, d = 1, q = 0, details = FALSE)
        
        return(as.data.frame(rw_drift_mod$ttable))
    }
    
    dat$Raw %>% 
        filter(Group == "Background") %>% 
        group_by(Well, Measurement) %>% 
        nest() %>% 
        mutate(results = map(data, add_drift_constant)) %>% 
        unnest(results)
}
