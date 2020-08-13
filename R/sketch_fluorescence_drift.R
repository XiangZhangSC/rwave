#' Visualization of fluorescence drift constants
#' 
#' A significant drift will be highlighted by a red dot, otherwise it is black
#'
#' @param df is a data frame produced by \code{qc_fluorescence_drift}
#'
#' @export
sketch_fluorescence_drift <- function(df) {
    ggplot(df, aes(as.factor(Measurement), Estimate)) + 
        geom_point() + 
        geom_point(data = filter(my_drift_constants, p.value < 0.05), color = "red") + 
        facet_wrap(~Well) + 
        labs(x = "Measurement", y = "Drift constant") + 
        theme_bw()
}
