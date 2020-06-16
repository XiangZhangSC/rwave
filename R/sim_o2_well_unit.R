#' Simulate the true O2 concentration of a well in a measurement period
#' 
#' sim_o2_well_unit simulates the true O2 concentration value in a well within 
#' a single measurement period
#' @param ocr the biological oxygen consumption
#' @param tick.start the tick number that the measurement period starts
#' @param tick.num the number of ticks in a measurement period
#' @import deSOlve

sim_o2_well_unit <- function(ocr, tick.start, tick.num) {
    TauP <- 43    # probe response time constant
    TauC <- 246   # From the wall to the medium
    TauAC <- 746  # From the atmosphere to the medium
    TauW <- 296   # From the medium to the wall
    o2_0 <- 214   # uM, Ambient o2 concentration required for pmol/min output
    
    parameters <- c(k_p = 1 / TauP, 
                    k_c = 1 / TauC, 
                    k_ac = 1 / TauAC, 
                    ocr = ocr,
                    k_w = 1 / TauW, 
                    k_aw = 0)
    
    state <- c(O2M = o2_0, 
               O2C = o2_0, 
               O2W = o2_0)
    
    gerencser <- function(t, state, parameters) {
        with(as.list(c(state, parameters)), {
            dO2M = k_p * (O2C - O2M)
            dO2C = k_c * (O2W - O2C) + k_ac * (o2_0 - O2C) - ocr
            dO2W = k_w * (O2C - O2W) + k_aw * (o2_0 - O2W)
            
            list(c(dO2M, dO2C, dO2W))
        })
    }
    # measurement period
    times <- seq(from = tick.start, length.out = tick.num)
    
    out <- ode(y = state, times = times, func = gerencser, parms = parameters) 
    return(out)
}
