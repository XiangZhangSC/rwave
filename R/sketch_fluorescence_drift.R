#' Visualization of fluorescence drift constants
#' 
#' A significant drift will be highlighted by a red dot, otherwise it is black
#'
#' @param df is a data frame produced by \code{qc_fluorescence_drift}
#' @param drift.constant is a logic value indicates whether only plot drift constants
#' @param num.ticks a numeric value produced by \code{summarize_assay}
#'
#' @export
sketch_fluorescence_drift <- function(df, drift.constant = TRUE, num.ticks) {
    
    if (drift.constant == TRUE) {
        ggplot(df, aes(as.factor(Measurement), Estimate)) + 
            geom_point() + 
            geom_point(data = filter(df, p.value < 0.05), color = "red") + 
            facet_wrap(~Well) + 
            labs(x = "Measurement", y = "Drift constant") + 
            theme_bw()
    } else {
        summarize_fluorescence_drift(df, num.ticks = num.ticks, stats.only = FALSE) %>% 
            ggplot(aes(as.factor(Tick2), fluorescence_drift)) + 
            geom_boxplot() + 
            facet_wrap(~Measurement) + 
            theme_bw() + 
            labs(x = "Tick", y = "Fluorescence drift")
    }
}
