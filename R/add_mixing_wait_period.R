#' Add extra ticks corresponding to the mixing & wait period
#'
#' @param dat.sim.init a data frame produced by \code{seed_rawdata_table}
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import purrr
#' @export
add_mixing_wait_period <- function(dat.sim.init, num.injections = 4) {
    
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # between the last point of a measurement phase 
    # and the first point of the next measurement phase 
    # there is a "mixing & wait period"
    mixing_wait_start <- dat.sim.init %>% 
        filter(Measurement < num.measurement) %>% 
        group_by(Measurement) %>% 
        summarize(time = max(time))
    
    mixing_wait_end <- dat.sim.init %>% 
        filter(Measurement > 1) %>% 
        group_by(Measurement) %>% 
        summarize(time = min(time))
    
    mixing_wait_period <- tibble(mixing_start = mixing_wait_start$time, 
                                 mixing_end = mixing_wait_end$time) %>% 
        mutate(time = map2(mixing_start, mixing_end, ~ seq(from = .x, to = .y, by = 1))) %>% 
        select(time) %>% 
        unnest(time)
    
    return(mixing_wait_period)
}
