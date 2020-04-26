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
    df1 <- df %>% 
      dplyr::filter(Group != "Background")
  } else {
    df1 <- df
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
      geom_vline(aes(xintercept = average_max_glycoAPR, color = Group), linetype = "dashed", size = 0.8) + 
      geom_hline(aes(yintercept = average_max_mitoAPR, color = Group), linetype = "dashed", size = 0.8) + 
      theme_classic() + 
      labs(x = "Glycolytic ATP Production Rate (pmol/min)", 
           y = "Mitochondrial ATP Production Rate (pmol/min)") + 
      coord_fixed()
  } else {
    ggplot(df1, aes(basal_glycoAPR, basal_mitoAPR, color = Group)) + 
      geom_point(size = 3, alpha = 0.5) + 
      scale_color_brewer("", palette = "Dark2") + 
      geom_vline(aes(xintercept = max_glycoAPR, color = Group), linetype = "dashed", alpha = 0.1) + 
      geom_hline(aes(yintercept = max_mitoAPR, color = Group), linetype = "dashed", alpha = 0.1) + 
      theme_classic() +
      labs(x = "Glycolytic ATP Production Rate (pmol/min)", 
           y = "Mitochondrial ATP Production Rate (pmol/min)") + 
      coord_fixed()
  }
}
