#' Seahorse radar chart
#' 
#' This function creates a Radar chart for a Seahorse assay
#' 
#' @param df a \code{data.frame} produced by \code{import_seahorse}
#' @param which_assay a \code{character} which can be "XFp", "XFe24", "XF" or "XFe96"
#' @param background a logical value indicates whetehr background is included in the plot
#' 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' 
#' @export
sketch_seahorse_radar <- function(df, which_assay, background = FALSE) {
  representive_ocr <- summarize_ocr(df)
  
  APR <- summarize_apr(df, which_assay = which_assay)
  
  df1 <- representive_ocr %>% 
    dplyr::select(Well, Group, proton_leak)
  
  df2 <- APR %>% 
    dplyr::select(Well, Group, basal_glycoAPR, max_glycoAPR, basal_mitoAPR, max_mitoAPR)
  
  df3 <- df1 %>% 
    inner_join(df2, by = c("Well", "Group")) %>% 
    rename(`Max GlycoAPR` = max_glycoAPR, 
           `Basal GlycoAPR` = basal_glycoAPR, 
           `Max mitoAPR` = max_mitoAPR, 
           `Basal mitoAPR` = basal_mitoAPR, 
           `Proton leak` = proton_leak)
  
  df4 <- df3 %>% 
    gather(what, value, -Well, -Group)
  
  df5 <- df4 %>% 
    group_by(what, Group) %>% 
    summarize(mean_val = mean(value))
  
  if (background == FALSE) {
    df6 <- df5 %>% 
      filter(Group != "Background")
  } else {
    df6 <- df5
  }
  
  ggplot(df6, aes(what, mean_val, color = Group)) + 
    geom_point() + 
    geom_polygon(aes(group = Group), fill = "transparent", alpha = 0.1) + 
    scale_fill_brewer("", palette = "Dark2") + 
    scale_color_brewer("", palette = "Dark2") + 
    labs(x = NULL, y = NULL) + 
    coord_polar() + 
    theme_minimal() + 
    theme(legend.position = "bottom", 
          axis.text.x = element_text(size = 14), 
          panel.grid = element_line(size = 1), 
          axis.text.y = element_blank())
} 
