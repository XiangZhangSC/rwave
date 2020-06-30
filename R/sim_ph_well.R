#' Simulate the true pH emission of a single well
#'
#' sim_ph_well simulates the pH emission fluoresence of a single well
#'
#' @param ocr a vector of true biological oxygen consumption rates, pmol O2 / min
#' @param glycolysis a vector of true glycolysis rates, mpH / min
#' @param tick.num the number of ticks within each measurement period
#' @import deSolve
#' @import dplyr
#' @import tibble
#' @import purrr
#' @import tidyr
#' @export

sim_ph_well <- function(ocr, 
                        glycolysis, 
                        tick.num = 15) {
    pH_0 <- 7.4
    
    num.injections <- length(glycolysis) - 1           # number of injections (not including background)
    num.measurement <- 3 * (num.injections + 1) # number of measurement periods
    
    dat.sim.init <- seed_rawdata_table(num.injections)
    
    # between the last point of a measurement phase 
    # and the first point of the next measurement phase 
    # there is a "mixing & wait period"
    mixing_wait_period <- add_mixing_wait_period(dat.sim.init, num.injections)
    
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
    probe_position <- navigate_probe_position(dat.sim.init, mixing_wait_period)
    
    # initial states
    state <- c(pH = pH_0)
    
    # time points that we want a solution from the ODE model
    times <- seq(from = 0, to = max(dat.sim.init$time), by = 1)
    
    # ODE model
    gerencser <- function(t, state, parameters) {
        with(as.list(c(state, parameters)), {
            ocr <- OCR(t)
            glycolysis <- Glycolysis(t)
            measuring <- probe_position(t)
            dpH = -(glycolysis / (1000 * 60) + BP * ocr * 10^-3 / 60 * max_H_per_O2 * 10^(pH - pK1)/(1 + 10^(pH - pK1))) * measuring + k_ph * (pH_0 - pH) * (1 - measuring)
            
            list(c(dpH), true_glycolysis = glycolysis)
        })
    }
    
    parameters <- c(max_H_per_O2 = 1, # the maximum H+ released per O2 consumed by respiration
                    pK1 = 6.093,      # pK for CO2 + H20 -> HCO3- + H+ at 37 degrees
                    BP = 0.0001,      # change in pH/nmol of H+
                    k_ph = 0.05)      # k_ac when the probe position is up 
    
    # Solve the ODE
    out <- ode(y = state, times = times, func = gerencser, parms = parameters) 
    dat.sim <- as.data.frame(out)
    
    
    # Convert to pH eimission
    pH_emission_simdat <- dat.sim %>% 
        mutate(true_emission_pH = (pH - 5.2) / 0.00007) %>% 
        tbl_df() %>% 
        right_join(dat.sim.init, by = "time") %>% 
        select(Measurement, Tick, everything())
    
    return(pH_emission_simdat)
}


