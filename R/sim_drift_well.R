#' Simulate fluorescent drift in temperature control wells
#'
#' Fluorescene intensity can vary over time in the blank wells
#'
#' @param num.injections the number of injections
#' @import deSolve
#' @import tidyr
#' @import dplyr
#' @export
sim_drift_well <- function(num.injections = 4) {
    dat.sim.init <- seed_rawdata_table(num.injections)
    mixing_wait <- add_mixing_wait_period(dat.sim.init, num.injections)
    probe_position <- navigate_probe_position(dat.sim.init, mixing_wait_period = mixing_wait)
    
    yini <- c(TW = 36, 
              FI = 12500)
    
    parms <- c(k_1 = 0.0006, 
               k_2 = 0.003)
    
    drift <- function(t, y, parms) {
        with(as.list(c(y, parms)), {
            measuring <- probe_position(t)
            dTW = -k_1 * (TW - 37) * measuring + -k_2 * (TW - 28) * (1 - measuring)
            dFI = -0.003 * FI * dTW
            
            list(c(dTW, dFI))
        })
    }
    
    times <- seq(from = 0, to = max(dat.sim.init$time), by = 1)
    out <- ode(y = yini, times = times, func = drift, parms = parms)
    
    out %>% 
        as.data.frame() %>% 
        rename(`Fluoresence intensity` = FI, 
               `Well temperature` = TW)
}
