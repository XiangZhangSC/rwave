#' Simulate the true O2 emission of a single well
#'
#' sim_o2_well simulates the O2 emission fluoresence of a single well
#'
#' @param ocr a vector of true biological oxygen consumption rates, pmol O2 / min
#' @param tick.num the number of ticks within each measurement period
#' @import deSolve
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
#' @export
sim_o2_well <- function(ocr, 
                        tick.num = 15) {
    TauP <- 43   # probe response time constant
    TauC <- 246  # From the wall to the medium
    TauAC <- 746 # From the atmosphere to the medium
    TauW <- 296  # From the medium to the wall
    o2_0 <- 214  # uM, Ambient o2 concentration after calibration
    
    num.injections <- length(ocr) - 1           # number of injections (not including background)
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # define the o2 data points to be simulated
    dat.sim.init <- seed_rawdata_table(num.injections)
    
    # between the last point of a measurement phase 
    # and the first point of the next measurement phase 
    # there is a "mixing & wait period"
    mixing_wait_period <- add_mixing_wait_period(dat.sim.init)
    
    # OCR(t)
    ## step 1 create a time-series
    true_ocr_df <- dat.sim.init %>% 
        mutate(OCR_t = rep(ocr, each = 3 * tick.num)) %>% 
        select(time, OCR_t)
    
    ## step 2 create the interpolating function
    OCR <- approxfun(true_ocr_df, rule = 2)
    
    # probe_position(t)
    probe_position <- navigate_probe_position(dat.sim.init, mixing_wait_period)
    
    # initial states
    state <- c(O2M = o2_0, 
               O2C = o2_0, 
               O2W = o2_0)
    
    # time points that we want a solution from the ODE model
    times <- seq(from = 0, to = max(dat.sim.init$time), by = 1)
    
    # ODE model
    gerencser <- function(t, state, parameters) {
        with(as.list(c(state, parameters)), {
            ocr <- OCR(t)
            measuring <- probe_position(t)
            dO2M = k_p * (O2C - O2M)
            dO2C = k_c * (O2W - O2C) + (k_ac * measuring + (1 - measuring) * k_ac_mixing) * (o2_0 - O2C) - ocr / (60 * Vc)
            dO2W = k_w * (O2C - O2W) + k_aw * (o2_0 - O2W)
            
            list(c(dO2M, dO2C, dO2W), true_OCR = ocr)
        })
    }
    
    parameters <- c(k_p = 1 / TauP, 
                    k_c = 1 / TauC, 
                    k_ac = 1 / TauAC, 
                    k_w = 1 / TauW, 
                    k_aw = 0, 
                    Vc = 20,            # Chamber volume ul
                    k_ac_mixing = 0.05) # k_ac when the probe position is up 
    
    # Solve the ODE
    out <- ode(y = state, times = times, func = gerencser, parms = parameters) 
    dat.sim <- as.data.frame(out)
    
    # Convert to O2 eimission
    # parameters to link o2 concentration to fluoresence
    F0 <- 54025.1441058472
    targetEmission <- 12500
    Ksv <- (1/o2_0)*(F0/targetEmission -1)
    
    # simulate the true fluoresence of a single well
    o2_emission_simdat <- dat.sim %>% 
        mutate(true_emission_o2 = map_dbl(O2C, ~ F0 / (.x * Ksv + 1))) %>% 
        tbl_df() %>% 
        right_join(dat.sim.init, by = "time") %>% 
        select(Measurement, Tick, everything())
    
    return(o2_emission_simdat)
}


