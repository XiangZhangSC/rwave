#' Summary of assay settings
#' 
#' summarize_assay returns assay settings such as number of ticks per measurements, number of measurements, etc.
#' 
#' @param dat a list produced by \code{import_seahorse}
#' @param what a character "measurement" or "tick"
#' @export
summarize_assay <- function(dat, what) {
    tot_num_ticks <- max(dat$Raw$Tick) + 1
    tot_num_measurements <- length(unique(dat$Raw$Measurement))
    
    num_tick_per_measurement <- tot_num_ticks / tot_num_measurements
    
    if (what == "tick") {
        return(num_tick_per_measurement)
    } else if (what == "measurement") {
        return(tot_num_measurements)
    }
}