#' Line plots of ECAR
#' 
#' This function draws line plots of EACR profile in different conditions
#' 
#' @import ggplot2
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_level a character either "well" or "group"
#' @param trend a logical value indicates whether the median is used to represent the trend
#' @return This function returns line plots of ECAR
#' @export
sketch_ecar <- function(df, which_level = "group", trend = TRUE) {
  if (which_level == "well") {
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
  } else if (which_level == "group") {
    df_group <- df %>% 
      select(Well, Group, Time, ECAR) %>% 
      group_by(Group, Time) %>% 
      summarize(mean_ecar = mean(ECAR), 
                sd_ecar = sd(ECAR)) %>% 
      ungroup()
    
    ggplot(df_group, aes(Time, mean_ecar, color = Group)) + 
      geom_line(aes(group = Group)) + 
      geom_pointrange(aes(ymin = mean_ecar - sd_ecar, ymax = mean_ecar + sd_ecar)) +
      scale_color_brewer(palette = "Set1") +
      labs(x = "Time (min)", y = "ECAR (mpH/min)") + 
      theme_classic() + 
      theme(legend.position = "top")
  }
  
}