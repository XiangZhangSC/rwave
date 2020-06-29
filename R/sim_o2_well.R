#' Simulate the true O2 emission of a single well
#'
#' sim_o2_well simulates the O2 emission fluoresence of a single well
#'
#' @param ocr a vector of true biological oxygen consumption rates, pmol O2 / min
#' @param glycolysis a vector of true glycolysis rates, mpH / min
#' @param tick.num the number of ticks within each measurement period
#' @param tick.interval the time interval between two ticks in a measurement period
#' @param mixing_wait_period the time interval between two measurement periods
#' @import deSolve
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr

sim_o2_well <- function(ocr, 
                        glycolysis, 
                        tick.num = 15, 
                        tick.interval = 14, 
                        mixing_wait_period = 120) {
    TauP <- 43   # probe response time constant
    TauC <- 246  # From the wall to the medium
    TauAC <- 746 # From the atmosphere to the medium
    TauW <- 296  # From the medium to the wall
    o2_0 <- 214  # uM, Ambient o2 concentration after calibration
    pH_0 <- 7.4
    
    num.injections <- length(ocr) - 1           # number of injections (not including background)
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    # measurement periods
    measurement_periods <- seq(from = 1, to = num.measurement, by = 1)
    
    # ticks along the measurement periods
    tick <- seq(from = 0, to = tick.num * num.measurement - 1, by = 1)
    
    # define the o2 data points to be simulated
    dat.sim.init <- data.frame(Measurement = rep(measurement_periods, each = tick.num), 
                               Tick = tick)
    # between the last point of a measurement phase 
    # and the first point of the next measurement phase 
    # there is a "mixing & wait period"
    dat.sim.init <- dat.sim.init %>% 
        mutate(time = tick.interval * tick + mixing_wait_period * (Measurement - 1))
                               
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
    
    # OCR(t)
    ## step 1 create a time-series
    true_ocr_df <- dat.sim.init %>% 
        mutate(OCR_t = rep(ocr, each = 3 * tick.num)) %>% 
        select(time, OCR_t)
    
    ## step 2 create the interpolating function
    OCR <- approxfun(true_ocr_df, rule = 2)
    
    # Glycolysis(t)
    true_glycolysis_df <- dat.sim.init %>% 
        mutate(glycolysis_t = rep(glycolysis, each = 3 * tick.num)) %>% 
        select(time, glycolysis_t)
    
    Glycolysis <- approxfun(true_glycolysis_df, rule = 2)
    
    # probe_position(t)
    ## When the probe is at the low position, measuring is on
    probe_position_df <- data.frame(
        time = seq(from = 0, to = max(dat.sim.init$time), by = 1), 
        measuring_position = 1L
    )
    ## When the probe is at the high position, measuring is off
    ## During this period, we assume that the diffusion rate from atomosphere to 
    ## chamber will be larger
    probe_position_df$measuring_position[probe_position_df$time %in% mixing_wait_period$time] <- 0L
    probe_position <- approxfun(probe_position_df, rule = 2)
    
    # initial states
    state <- c(O2M = o2_0, 
               O2C = o2_0, 
               O2W = o2_0, 
               pH = pH_0)
    
    # time points that we want a solution from the ODE model
    times <- seq(from = 0, to = max(dat.sim.init$time), by = 1)
    
    # ODE model
    gerencser <- function(t, state, parameters) {
        with(as.list(c(state, parameters)), {
            ocr <- OCR(t)
            glycolysis <- Glycolysis(t)
            measuring <- probe_position(t)
            dO2M = k_p * (O2C - O2M)
            dO2C = k_c * (O2W - O2C) + (k_ac * measuring + (1 - measuring) * k_ac_mixing) * (o2_0 - O2C) - ocr / (60 * Vc)
            dO2W = k_w * (O2C - O2W) + k_aw * (o2_0 - O2W)
            dpH = -(glycolysis / (1000 * 60) + BP * ocr * 10^-3 / 60 * max_H_per_O2 * 10^(pH - pK1)/(1 + 10^(pH - pK1))) * measuring + k_ph * (pH_0 - pH) * (1 - measuring)
            
            list(c(dO2M, dO2C, dO2W, dpH), true_OCR = ocr, true_glycolysis = glycolysis)
        })
    }
    
    parameters <- c(k_p = 1 / TauP, 
                    k_c = 1 / TauC, 
                    k_ac = 1 / TauAC, 
                    k_w = 1 / TauW, 
                    k_aw = 0, 
                    max_H_per_O2 <- 1,  # the maximum H+ released per O2 consumed by respiration
                    pK1 <- 6.093,       # pK for CO2 + H20 -> HCO3- + H+ at 37 degrees
                    Vc <- 20,           # Chamber volume ul
                    BP = 0.0001,        # change in pH/nmol of H+
                    k_ph = 0.05,        # k_ph normalize pH by re-mixing buffer
                    k_ac_mixing = 0.15) # k_ac when the probe position is up 
    
    # Solve the ODE
    out <- ode(y = state, times = times, func = gerencser, parms = parameters) 
    dat.sim <- as.data.frame(out)
        
    
    # Convert to O2 eimission
    # parameters to link o2 concentration to fluoresence
    F0 <- 54025.1441058472
    targetEmission <- 12500
    Ksv <- (1/o2_0)*(F0/targetEmission -1)
    
    # simulate the true fluoresence of a single well
    seahorse_data_sim <- dat.sim %>% 
        mutate(true_emission_o2 = map_dbl(O2C, ~ F0 / (.x * Ksv + 1)), 
               true_emission_pH = map_dbl((pH - 5.2) / 0.00007)) %>% 
        tbl_df() %>% 
        right_join(dat.sim.init, by = "time") %>% 
        select(Measurement, Tick, everything())
    
    return(seahorse_data_sim)
}


