#' Add true OCR to every well in a plate
#'
#' add_true_ocr spikes in true biological OCR in every well
#'
#' @param plate_layout is a data.frame produced by \code{seed_plate}
#' @param baseline_ocr is a vector of true biological OCR without any injection
#' @export
add_true_ocr <- function(plate_layout, 
                         baseline_ocr, 
                         num.injections = 4) {
    baseline_ocr.df <- tibble(Group = unique(plate_layout$Group), 
                              true_baseline_ocr = baseline_ocr)
    
    
    injection.id <- paste("Injection", seq(from = 1, to = num.injections, by = 1))
    
    # when an injection was added the biological OCR is boosted or suppressed
    # the effect is the same for different groups
    # No injection during the 1st measurement period
    # After injection, the OCR cannot be lower than the baseline
    deviation_ocr_injection <- tibble(Injection = c('No injection', injection.id), 
                                      deviation_ocr_injection = c(0, rnorm(num.injections, mean = 0, sd = 10)))
    
    plate.sim <- plate_layout %>% 
        left_join(baseline_ocr.df, by = "Group") %>% 
        mutate(deviation_ocr_injection = list(deviation_ocr_injection)) %>% 
        unnest(deviation_ocr_injection)
    
    empty_well <- plate.sim$Group == "Background"
    
    plate.sim$true_baseline_ocr[empty_well] <- 0
    plate.sim$deviation_ocr_injection[empty_well] <- 0
    
    plate.sim %>% 
        mutate(true_ocr = true_baseline_ocr + deviation_ocr_injection) %>% 
        group_by(Well, Group, true_baseline_ocr) %>% 
        nest() %>% 
        ungroup()
}
