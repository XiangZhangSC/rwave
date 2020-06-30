#' Initialization of data table for recording raw data
#'
#' @param num.injections the number of injections used in Seahorse XF analysis
#' @param tick.num the number of ticks within each measurement period
#'
#'

seed_rawdata_table <- function(num.injections, tick.num = 15) {
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # measurement periods
    measurement_periods <- seq(from = 1, to = num.measurement, by = 1)
    
    # ticks along the measurement periods
    tick <- seq(from = 0, to = tick.num * num.measurement - 1, by = 1)
    
    # define the o2 data points to be simulated
    data.frame(Measurement = rep(measurement_periods, each = tick.num), 
               Tick = tick)
    
}

