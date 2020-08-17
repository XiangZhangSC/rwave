#' Summarize fluoresence drift
#' 
#' @param df a data frame produced by \code{qc_fluorescence_drift}
#' @param num.ticks the number of ticks in a measurement period produced by \code{summarize_assay}
#' @param stats.only a logical value whether only the median of temperature controlled wells are shown
#' @import dplyr
#' @import tidyr
#' @export

summarize_fluorescence_drift <- function(df, num.ticks, stats.only = TRUE) {
    
    # Recount the tick number whenever a new measurement period starts
    dat <- df %>% 
        ungroup() %>% 
        unnest(data) %>%
        mutate(Tick2 = Tick - num.ticks * (Measurement - 1))
    
    # The first fluorescence value is supposed to be not affected by drift
    F0_dat <- dat %>% 
        filter(Tick2 == 0) %>% 
        select(Measurement, Well, `O2 Corrected Em.`) %>% 
        rename(F0 = `O2 Corrected Em.`)
    
    # The actual fluorescence values over ticks
    Ft_dat <- dat %>%
        select(Measurement, Well, Tick2, `O2 Corrected Em.`) %>% 
        rename(Ft = `O2 Corrected Em.`)
    
    # The fluorescence drift
    drift_dat <- Ft_dat %>% 
        left_join(F0_dat, by = c("Measurement", "Well")) %>% 
        mutate(fluorescence_drift = Ft - F0)
    
    if (stats.only == TRUE) {
        drift_dat %>% 
            group_by(Measurement, Tick2) %>% 
            summarize(average_drift_amount = median(fluorescence_drift))
    } else {
        return(drift_dat)  
    }
}
