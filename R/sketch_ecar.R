#' Line plots of ECAR
#' 
#' This function draws line plots of EACR profile in different conditions
#' 
#' @import ggplot2
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param trend a logical value indicates whether the median is used to represent the trend
#' @return This function returns line plots of ECAR
#' @export
sketch_ecar <- function(df, trend = TRUE) {
  g0 <- ggplot(df, aes(Time, ECAR)) + 
    facet_wrap(~Group) + 
    labs(x = "Time (min)", y = "ECAR (mpH/min)") + 
    theme_classic()
  
  if (trend == TRUE) {
    g0 + geom_line(aes(group = Well), alpha = 0.2) + 
      stat_summary(fun = median, geom = "line", aes(group = Group))
  } else {
    g0 + geom_line(aes(group = Well))
  }
}