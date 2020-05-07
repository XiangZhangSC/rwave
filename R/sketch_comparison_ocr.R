#' Visualize OCR comparisons between groups
#' 
#' \code{sketch_comparison_ocr} creates a dot plot showing OCR difference between groups
#' 
#' @param df a \code{data.frame} resulted by \code{compare_ocr}
#' @param which_apr a \code{character} or a vector of \code{character} indicates the name(s) of OCR to be compared
#' 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import stringr
#' 
#' @export
sketch_comparison_ocr <- function(df, which_ocr) {
  p <- df %>% 
    select(what, data) %>% 
    unnest(data) %>% 
    filter(what %in% which_ocr) %>% 
    ggplot(aes(Group, val)) + 
    geom_point(position = position_jitter(width = 0.2), alpha = 0.3) + 
    stat_summary(fun = mean, na.rm = TRUE, color = "red", geom = "point") + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), na.rm = TRUE, color = "red", geom = "errorbar", width = 0.1) + 
    labs(x = NULL, y = NULL) + 
    theme_classic()
  
  if (length(which_ocr) == 1) {
    p
  } else {
    p + facet_wrap(~what, scales = "free")
  }
}
