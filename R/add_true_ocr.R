#' Add true OCR to every well in a plate
#'
#' add_true_ocr spikes in true biological OCR in every well
#'
#' @param plate_layout is a data.frame produced by \code{seed_plate}
#' @param ocr_per_cell true biological OCR of a single cell without any injection
#' @export
add_true_ocr <- function(plate_layout, 
                         ocr_per_cell) {
    plate.sim <- plate_layout %>% 
        mutate(true_baseline_ocr = ocr_per_cell * cell_n)
    
    # when an injection was added the biological OCR is boosted or suppressed
    # No injection during the 1st measurement period
    # After injection, the OCR cannot be lower than the baseline
    plate.sim <- plate.sim %>% 
        mutate(deviation_ocr_injection = map(what_test, add_ocr_deviation_injection)) %>% 
        unnest(deviation_ocr_injection)
    
    empty_well <- plate.sim$Group == "Background"
    
    plate.sim$deviation_ocr_injection[empty_well] <- 0
    
    plate.sim %>% 
        mutate(true_ocr = true_baseline_ocr + deviation_ocr_injection, 
               true_ocr = ifelse(true_ocr < 0, 0, true_ocr)) %>% 
        group_by(Well, Group, cell_n, true_baseline_ocr) %>% 
        nest() %>% 
        ungroup()
}
