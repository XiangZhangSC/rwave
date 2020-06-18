#' Simulate the O2 emission of a Seahorse assay plate
#'
#' sim_o2_plate simulated the O2 emission in a 96-well plate. 
#' The baseline true biological oxygen consumption rate is drawn from a uniform distribution. 
#' 
#' @param experiment_setup a data.frame with two columns, group label and number of replicates
#' @param between_well_variation the standard deviation of fluoresence between different wells
#' @param within_well_variation the standard deviation of fluoresence within a well
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
sim_o2_plate <- function(experiment_setup, 
                         between_well_variation, 
                         within_well_variation, 
                         num.injections = 4, 
                         min.ocr = 0.1, 
                         max.ocr = 0.3, 
                         background.ocr = 0.1) {
    
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
    
    # Depsite calibration, there is a long period before the 1st measurement.
    # As a consequence, 10,000 fluoresence units can mean different levels of O2 
    # in different wells. 
    num.well <- length(well.id)
    bias_well <- rnorm(num.well, mean = 0, sd = between_well_variation)
    
    well_property <- experiment_setup %>% 
        mutate(baseline_ocr = runif(num.injections, min = min.ocr, max = max.ocr), 
               group_label = map2(Group, n, ~rep(.x, each = .y)), 
               true_ocr = map(baseline_ocr, ~ .x * c(1, effect_size_injections))) %>% 
        select(group_label, true_ocr) %>% 
        unnest(group_label)
    
    # initialize the simulated plate data
    plate.sim <- tibble(Well = well.id, 
                        Group = "From which group?", 
                        true_ocr = list(rep(background.ocr, times = num.injections + 1)), 
                        bias_well = bias_well)
    
    plate.sim$Group[corner_wells] <- "Background"
    plate.sim$Group[non_corner_wells] <- well_property$group_label
    plate.sim$true_ocr[non_corner_wells] <- well_property$true_ocr
    
    
    # simulate the true o2 and corresponding fluoresence data
    plate.errorfree <- plate.sim %>% 
        mutate(simulated_data = map(true_ocr, sim_o2_well)) %>% 
        select(Well, Group, simulated_data, bias_well) %>% 
        unnest(simulated_data)
    
    # Every time the sensor registered a value, there is a measurement error
    # We assume that every registration is independent
    num.registration <- nrow(plate.errorfree)
    measurement_error <- rnorm(num.registration, mean = 0, sd = within_well_variation)
    
    plate.errorfree$measurement_error <- measurement_error
    
    # add the between well and within well variation to the true fluoresence
    plate.sim <- plate.errorfree %>% 
        mutate(`O2 Corrected Em.` = true_fluoresence + bias_well + measurement_error)
    
    return(plate.sim)
}
