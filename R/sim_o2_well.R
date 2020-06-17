#' Simulate the true O2 emission of a single well
#'
#' sim_o2_well simulates the O2 emission fluoresence of a single well
#'
#' @param ocr a vector of true biological oxygen consumption rates
#' @param mixing an event data.frame containing the state variable that is to be manipulated
#' @param tick.num the number of ticks within each measurement period
#' @param tick.interval the time interval between two ticks in a measurement period
#' @import deSolve
#' @import dplyr
#' @import tibble

sim_o2_well <- function(ocr, tick.num = 15, tick.interval = 14) {
    TauP <- 43    # probe response time constant
    TauC <- 246   # From the wall to the medium
    TauAC <- 746  # From the atmosphere to the medium
    TauW <- 296   # From the medium to the wall
    o2_0 <- 214   # uM, Ambient o2 concentration after calibration
    
    
    num.injections <- length(ocr) - 1           # number of injections (not including background)
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # measurement periods
    measurement_periods <- seq(from = 1, to = num.measurement, by = 1)
    
    # ticks along the measurement periods
    tick <- seq(from = 0, to = tick.num * num.measurement - 1, by = 1)
    
    # define the o2 data points to be simulated
    dat.sim.init <- data.frame(Measurement = rep(measurement_periods, each = tick.num), 
                               tick = tick, 
                               time = tick.interval * tick)
    
    # OCR(t)
    ## step 1 create a time-series
    true_ocr_df <- dat.sim.init %>% 
        mutate(OCR_t = rep(ocr, each = 3 * tick.num)) %>% 
        select(time, OCR_t)
    
    ## step 2 create the interpolating function
    OCR <- approxfun(true_ocr_df, rule = 2)
    
    state <- c(O2M = o2_0, 
               O2C = o2_0, 
               O2W = o2_0)
    
    times <- dat.sim.init$time
    
    gerencser <- function(t, state, parameters) {
        with(as.list(c(state, parameters)), {
            ocr <- OCR(t)
            dO2M = k_p * (O2C - O2M)
            dO2C = k_c * (O2W - O2C) + k_ac * (o2_0 - O2C) - ocr
            dO2W = k_w * (O2C - O2W) + k_aw * (o2_0 - O2W)
            
            list(c(dO2M, dO2C, dO2W), true_OCR = ocr)
        })
    }
    
    parameters <- c(k_p = 1 / TauP, 
                    k_c = 1 / TauC, 
                    k_ac = 1 / TauAC, 
                    k_w = 1 / TauW, 
                    k_aw = 0)
    
    # Events
    event_times <- dat.sim.init %>% 
        filter(Measurement > 1) %>% 
        group_by(Measurement) %>% 
        summarize(time = min(time))
    
    ## Starting from the 2nd measurement
    ## O2C at the first tick will be forced to be ambient O2
    mixing <- data.frame(
        var = "O2C", 
        time = event_times$time, 
        value = o2_0, 
        method = "replace"
    )
    
    out <- ode(y = state, times = times, func = gerencser, parms = parameters, events = list(data = mixing)) 
    dat.sim <- as.data.frame(out)
    
    # parameters to link o2 concentration to fluoresence
    O2_0_mmHg <-151.6900241
    F0 <- 54025.1441058472
    targetEmission <- 12500
    Ksv <- (1/O2_0_mmHg)*(F0/targetEmission -1)
    
    # simulate the true fluoresence of a single well
    dat.sim %>% 
        mutate(true_fluoresence = map_dbl(O2M, ~ F0 / (.x * Ksv + 1))) %>% 
        tbl_df()
}


