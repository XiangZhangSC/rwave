#' OCR comparison between groups
#' 
#' compare_ocr compares OCR between experimental groups
#' 
#' When there are two groups, the Wilcoxon signed-rank test will be used for the group comparison. 
#' When there are three groups, the Kruskal–Wallis one-way analysis of variance will be sued for the group comparison.
#' 
#' @param df a \code{data.frame} with raw data and produced by \code{import_seahorse}
#' @param stats.only a \code{logical} value that indicates whether only the statistical outcomes are returned
#' 
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import broom
#' 
#' @export

compare_ocr <- function(df, stats.only = FALSE) {
  
  # calculate APR
  ocr_df <- summarize_ocr(df)
  
  n_groups <- length(unique(ocr_df$Group))
  
  ocr_nest <- ocr_df %>% 
    gather(what, val, -Group, -Well) %>% 
    filter(Group != "Background") %>% 
    group_by(what) %>% 
    nest()
  
  if (n_groups >= 3) {
    ocr_res <- ocr_nest %>% 
      mutate(stat = map(data, ~tidy(kruskal.test(val~Group, data = .))))
  } else {
    ocr_res <- ocr_nest %>% 
      mutate(stat = map(data, ~tidy(wilcox.test(val~Group, data = .))))
  }
  
  # output
  
  if (stats.only == TRUE) {
    ocr_res %>% 
      select(-data) %>% 
      unnest(stat)
  } else {
    ocr_res
  }
}
