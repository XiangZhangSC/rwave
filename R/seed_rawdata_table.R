#' Initialization of data table for recording raw data
#'
#' @param num.injections the number of injections used in Seahorse XF analysis
#' @param tick.num the number of ticks within each measurement period
#' @param tick.interval the time interval between two ticks in a measurement period, seconds
#' @param mixing_wait_duration the time interval between two measurement periods, seconds
#'
#' @import dplyr
#' @export

seed_rawdata_table <- function(num.injections, tick.num = 15, tick.interval = 14, mixing_wait_duration = 120) {
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # measurement periods
    measurement_periods <- seq(from = 1, to = num.measurement, by = 1)
    
    # ticks along the measurement periods
    tick <- seq(from = 0, to = tick.num * num.measurement - 1, by = 1)
    
    # define the o2 data points to be simulated
    dat.sim.init <- data.frame(Measurement = rep(measurement_periods, each = tick.num), 
               Tick = tick)
    
    dat.sim.init <- dat.sim.init %>% 
        mutate(time = tick.interval * Tick + mixing_wait_duration * (Measurement - 1))
    
    return(dat.sim.init)
}

