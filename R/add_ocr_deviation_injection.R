#' Add effects of injections on OCR
#'
#' @param stress_test the stress test name, example "mito"
#' @import tibble
#'
add_ocr_deviation_injection <- function(stress_test = "mito") {
    if (stress_test == "mito") {
        injections <- c("Basal", "OM", "FCCP", "RO")
        
        bOM <- rnorm(1, mean = -22.9, sd = 4.63)
        bFCCP <- rnorm(1, mean = 21.8, sd = 7.05)
        bRO <- rnorm(1, mean = -28.0, sd = 4.39)
        
        tibble(Injection = injections, 
               deviation_ocr_injection = c(0, bOM, bFCCP, bRO))
    }
    
}
