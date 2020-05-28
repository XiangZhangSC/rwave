#' Line plots of OCR
#' 
#' This function draws line plots of OCR profile in different conditions
#' 
#' @import ggplot2
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_level a character either "data" or "group"
#' @param trend a logical value indicates whether the median of replicates will be used
#' @return This function returns line plots of OCR
#' @export
sketch_ocr <- function(df, which_level = "group", trend = TRUE) {
  if (which_level == "data") {
    g0 <- ggplot(df, aes(Time, OCR)) + 
      facet_wrap(~Group) + 
      labs(x = "Time (min)", y = "OCR (pmol/min)") + 
      theme_classic()
    
    if (trend == TRUE) {
      g0 + geom_line(aes(group = Well), alpha = 0.2) + 
        stat_summary(fun = median, geom = "line", aes(group = Group))
    } else {
      g0 + geom_line(aes(group = Well))
    }
  } else if (which_level == "group") {
    df_group <- df %>% 
      select(Well, Group, Time, OCR) %>% 
      group_by(Group, Time) %>% 
      summarize(mean_ocr = mean(OCR), 
                sd_ocr = sd(OCR)) %>% 
      ungroup()
    
    ggplot(df_group, aes(Time, mean_ocr, color = Group)) + 
      geom_line(aes(group = Group)) + 
      geom_pointrange(aes(ymin = mean_ocr - sd_ocr, ymax = mean_ocr + sd_ocr)) + 
      scale_color_brewer(palette = "Set1") +
      labs(x = "Time (min)", y = "OCR (pmol/min)") + 
      theme_classic() + 
      theme(legend.position = "top")
  }
  
}