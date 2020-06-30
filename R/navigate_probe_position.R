#' Define probe position for every tick
#'
#' @param dat.sim.init a data frame that is produced by \code{seed_rawdata_table}
#' @param mixing_wait_period a data frame that is produced by \code{add_mixing_wait_period}
#' @export

navigate_probe_position <- function(dat.sim.init, mixing_wait_period) {
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
    
    return(probe_position)
}
