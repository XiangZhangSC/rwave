#' Visualize the experiment setup in a plate
#'
#' sketch_plate_layout shows the group assignment in every well
#' 
#' @param experiment_setup a data.frame with two columns, group label and number of replicates
#'
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @export
sketch_plate_layout <- function(experiment_setup) {
    seed_plate(experiment_setup) %>% 
        mutate(well.row = str_sub(Well, 1, 1), 
               well.col = str_sub(Well, 2, 3)) %>% 
        ggplot(aes(well.col, well.row, color = Group)) + 
        geom_text(aes(label = Group)) + 
        labs(x = NULL, y = NULL) + 
        theme_bw() + 
        theme(legend.position = "none") + 
        scale_color_brewer(palette = "Dark2")
}