#' Simulate the true O2 emission of a single well
#'
#' sim_o2_plate simulates the O2 emission fluoresence of a single well
#'
#' @param ocr a vector of true biological oxygen consumption rates
#' @param num.tick the number of ticks within each measurement period
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import purrr

sim_o2_well <- function(ocr, num.tick = 15) {
    
    
    num.injections <- length(ocr) - 1           # number of injections (not including background)
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # measurement periods
    measurement_periods <- seq(from = 1, to = num.measurement, by = 1)
    
    # tick
    tick <- seq(from = 0, to = num.tick * num.measurement - 1, by = 1)
    
    dat.sim.init <- tibble(Measurement = rep(measurement_periods, each = num.tick), 
                           which.tick = tick)
    
    dat.sim <- dat.sim.init %>% 
        group_by(Measurement) %>% 
        summarize(tick.start = min(which.tick))
    
    # Every phase consists of three measurements
    spiked_in_ocrs <- rep(ocr, each = 3)
    
    # parameters to link o2 concentration to fluoresence
    O2_0_mmHg <-151.6900241
    F0 <- 54025.1441058472
    targetEmission <- 12500
    Ksv <- (1/O2_0_mmHg)*((F0/targetEmission) -1)
    
    # simulate the true fluoresence of a single well
    dat.sim %>% 
        mutate(true_ocr = spiked_in_ocrs, 
               true_o2 = map2(true_ocr, tick.start, ~ sim_o2_well_unit(ocr = .x, tick.start = .y, tick.num = num.tick)), 
               true_o2 = map(true_o2, as.data.frame)) %>% 
        unnest(true_o2) %>% 
        mutate(true_fluoresence = map_dbl(O2M, ~ F0 / (.x * Ksv + 1)))
        
}

