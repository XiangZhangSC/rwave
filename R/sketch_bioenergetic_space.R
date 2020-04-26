#' Visualization of the bioenergetic space
#' 
#' This function plots mitoAPR against glycoAPR
#' 
#' @param df a \code{data.frame} produced by \code{summarize_apr}
#' @param average a logical value indcates whether only to show the average of technical replicates
#' @param background a logical value indicates whetehr background is included in the plot
#' @import ggplot2
#' @import dplyr
#' 
#' @export

sketch_bioenergetic_space <- function(df, average = TRUE, background = FALSE) {
  
  bioenergetic_dat <- df %>% 
    dplyr::select(Well, Group, basal_mitoAPR, max_mitoAPR, basal_glycoAPR, max_glycoAPR)
  
  if (background == FALSE) {
    df1 <- bioenergetic_dat %>% 
      dplyr::filter(Group != "Background")
  } else {
    df1 <- bioenergetic_dat
  }
  
  if (average == TRUE) {
    df2 <- df1 %>% 
      group_by(Group) %>% 
      summarize(average_glycoAPR = mean(basal_glycoAPR), 
                se_glycoAPR = sd(basal_glycoAPR)/ sqrt(n()), 
                average_mitoAPR = mean(basal_mitoAPR), 
                se_mitoAPR = sd(basal_mitoAPR)/ sqrt(n()), 
                average_max_glycoAPR = mean(max_glycoAPR), 
                average_max_mitoAPR = mean(max_mitoAPR))
    
    ggplot(df2, aes(x = average_glycoAPR, y = average_mitoAPR, color = Group)) + 
      geom_point(size = 3, shape = 1) + 
      geom_errorbarh(aes(xmin = average_glycoAPR - se_glycoAPR, xmax = average_glycoAPR + se_glycoAPR), height = 5) + 
      geom_errorbar(aes(ymin = average_mitoAPR - se_mitoAPR, ymax = average_mitoAPR + se_mitoAPR), width = 5) + 
      scale_color_brewer("", palette = "Dark2") + 
      geom_rect(aes(xmin = 0, xmax = average_max_glycoAPR, ymin = 0, ymax = average_max_mitoAPR), 
                fill = "transparent", 
                linetype = "dashed") + 
      theme_classic() + 
      labs(x = "Glycolytic ATP Production Rate (pmol/min)", 
           y = "Mitochondrial ATP Production Rate (pmol/min)") + 
      coord_fixed()
  } else {
    ggplot(df1, aes(basal_glycoAPR, basal_mitoAPR)) + 
      geom_point(size = 3, alpha = 0.5) + 
      geom_rect(aes(xmin = 0, xmax = max_glycoAPR, ymin = 0, ymax = max_mitoAPR), 
                color = "black", 
                fill = "transparent", 
                linetype = "dashed", 
                size = 0.1) + 
      theme_classic() +
      labs(x = "Glycolytic ATP Production Rate (pmol/min)", 
           y = "Mitochondrial ATP Production Rate (pmol/min)") + 
      facet_wrap(~Group, scale = "free")
  }
}
