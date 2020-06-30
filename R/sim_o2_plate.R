#' Simulate the O2 emission of a Seahorse assay plate
#'
#' sim_o2_plate simulated the O2 emission in a 96-well plate with systematic 
#' errors due to well and injection
#' 
#' @param experiment_setup a data.frame with two columns, group label and number of replicates
#' @param baseline_ocr a vector of true biological OCR values without any injection
#' @param num.injections the number of injections will be used in the seahorse assay
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
#' @export
sim_o2_plate <- function(experiment_setup, 
                         baseline_ocr, 
                         num.injections = 4) {
    
    # pipette cells from different experimental group into a 96-well plate
    plate_layout <- seed_plate(experiment_setup)
    
    # cells cultured with different experimental groups may have different 
    # true biological OCR corresponding to different ture o2 emission
    plate.sim <- plate_layout %>% 
        add_true_ocr(baseline_ocr) %>% 
        mutate(simulated_data = map(data, ~sim_o2_well(.x$true_ocr)))
    
    ## There is a long period before the 1st measurement.
    ## As a consequence, 10,000 fluoresence units can mean different levels of O2 
    ## in different wells. 
    plate.sim$deviation_fluoresence_well <- rnorm(96, mean = 0, sd = 1000)
    
    plate.sim.long <- plate.sim %>% 
        select(-data) %>% 
        unnest(simulated_data)
    
    ## Sometimes after an injection, all the fluoresence values afterwards will be 
    ## larger or smaller than what should be 
    deviation_fluoresence_injection.df <- add_fluoresence_deviation_injection(plate_layout)
    
    deviation_fluoresence_injection.df.long <- deviation_fluoresence_injection.df %>% 
        unnest(deviation_fluoresence_injection)
    
    plate.sim.complete.long <- plate.sim.long %>% 
        left_join(deviation_fluoresence_injection.df.long, by = c("Well", "Group", "Measurement", "Tick"))
    
    # Every time the sensor registered a value, there is a measurement error
    # We assume that every registration is independent
    plate.sim.final <- plate.sim.complete.long %>% 
        mutate(`O2 Corrected Em.` = pmap_dbl(list(true_emission_o2, deviation_fluoresence_well, deviation_fluoresence_injection), 
                                            ~ rnorm(1, 
                                                    mean = ..1 + ..2 + ..3, 
                                                    sd = 10)))
    
    return(plate.sim.final)
}
