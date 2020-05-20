#' Visualize the raw O2 pressure data
#'
#' sketch_o2 visualizes the raw measurements of O2 pressure 
#'
#' @param df a \code{data frame} containing raw data loaded by \code{import_seahorse}
#' @import dplyr
#' @import ggplot2
#' @export
sketch_o2 <- function(df) {
  ggplot(df, aes(Tick, `O2 (mmHg)`)) + 
    geom_line(aes(group = Well), alpha = 0.1) + 
    stat_summary(fun = median, geom = "line", color = "steelblue") + 
    facet_wrap(~Group) + 
    theme_classic()
}
