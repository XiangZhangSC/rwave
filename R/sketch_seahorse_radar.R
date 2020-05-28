#' Seahorse radar chart
#' 
#' This function creates a Radar chart for a Seahorse assay
#' 
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_assay a character which can be "XFp", "XFe24", "XF" or "XFe96"
#' @param include.background a logical value indicates whetehr background is included in the plot
#' 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' 
#' @export
sketch_seahorse_radar <- function(df, which_assay, include.background = FALSE) {
  
  redar_df <- summarize_seahorse(df, which_assay = which_assay)
  
  if (include.background == FALSE) {
    redar_df <- redar_df %>% 
      filter(Group != "Background")
  }
  
  redar_df %>% 
    gather(what, mean_val, -Group) %>% 
    ggplot(aes(what, mean_val, color = Group)) + 
    geom_point() + 
    geom_polygon(aes(group = Group), fill = "transparent", alpha = 0.1) + 
    scale_color_brewer("", palette = "Set1") + 
    labs(x = NULL, y = NULL) + 
    coord_polar() + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 14), 
          panel.grid = element_line(size = 1), 
          axis.text.y = element_blank())
} 
