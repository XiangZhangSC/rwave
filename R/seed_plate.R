#' Initialize the Seahorse XF output
#'
#' seed_plate produces a data frame with two columns Well and group Tag.
#' 
#' @param experiment_setup a data frame with two columns Group and n
#' @param cell_number_per_well the targeted cell counts per well
#' @import tibble
#'
#' @export

seed_plate <- function(experiment_setup, cell_number_per_well) {
    # plate set up
    well.row <- LETTERS[1:8]
    
    well.col <- as.character(seq(from = 1, to = 12, by = 1))
    well.col <- str_pad(well.col, width = 2, side = "left", pad = "0")
    
    plate <- expand.grid(well.row, well.col)
    colnames(plate) <- c("row_id", "column_id")
    well.id <- paste0(plate$row_id, plate$column_id)
    
    # The four corner wells are "background"
    corner_wells <- well.id %in% c("A01", "A12", "H01", "H12")
    non_corner_wells <- !corner_wells
    
    # initialize the simulated plate data
    plate.sim <- tibble(Well = well.id, 
                        Group = "From which group?", 
                        cell_n = 0)
    
    plate.sim$Group[corner_wells] <- "Background"
    
    # every well represents a sample from a specific experimental group
    plate.sim$Group[non_corner_wells] <- tag_well(experiment_setup)
    
    # there is no cells in corner wells
    # for other non corner wells, the cell numbers fluctuate around the target number
    plate.sim$cell_n[non_corner_wells] <- rpois(92, lambda = cell_number_per_well)
    
    return(plate.sim)
}
