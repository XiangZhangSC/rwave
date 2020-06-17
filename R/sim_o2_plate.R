#' Simulate the true O2 emission of a Seahorse assay plate
#'
#' sim_o2_plate simulated the true O2 emission fluoresence in a 96-well plate. 
#' The baseline true biological oxygen consumption rate is drawn from a uniform distribution. 
#' 
#' @param experiment_setup a data.frame with two columns, group label and number of replicates
#' @param num.injections the number of injections will be used in the seahorse assay
#' @param min.ocr the minimum true biological oxygen consumption rate
#' @param max.ocr the maximum true biological oxygen consumption rate
#' @param background.ocr the true biological oxygen consumption rate in the background wells
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @export
sim_o2_plate <- function(experiment_setup, num.injections = 4, min.ocr = 0.1, max.ocr = 0.3, background.ocr = 0.1) {
    
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
    
    # every well represents a sample from a specific experimental group
    # when an injection was added the biological OCR is boosted or suppressed
    # the effect is the same for different groups
    effect_size_injections = runif(num.injections, min = 0, max = 2)
    
    well_property <- experiment_setup %>% 
        mutate(baseline_ocr = runif(num.injections, min = min.ocr, max = max.ocr), 
               group_label = map2(Group, n, ~rep(.x, each = .y)), 
               true_ocr = map(baseline_ocr, ~ .x * c(1, effect_size_injections))) %>% 
        select(group_label, true_ocr) %>% 
        unnest(group_label)
    
    # initialize the simulated plate data
    plate.sim <- tibble(Well = well.id, 
                        Group = "From which group?", 
                        true_ocr = list(rep(background.ocr, times = num.injections + 1)))
    
    plate.sim$Group[corner_wells] <- "Background"
    plate.sim$Group[non_corner_wells] <- well_property$group_label
    plate.sim$true_ocr[non_corner_wells] <- well_property$true_ocr
    
    # simulate the true o2 and corresponding fluoresence data
    plate.sim <- plate.sim %>% 
        mutate(simulated_data = map(true_ocr, sim_o2_well)) %>% 
        select(Well, Group, simulated_data) %>% 
        unnest(simulated_data)
    
}
