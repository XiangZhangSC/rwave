#' OCR comparison between groups
#' 
#' compare_ocr compares OCR between experimental groups
#' 
#' When there are two groups, the Wilcoxon signed-rank test will be used for the group comparison. 
#' When there are three groups, the Kruskal–Wallis one-way analysis of variance will be sued for the group comparison.
#' 
#' @param df a \code{data.frame} with raw data and produced by \code{import_seahorse}
#' @param robust a logical variable which indicates non-parametric tests will be used if it is TRUE by default.
#' when robust = FALSE, t test or anova will be used depending on the number of groups
#' @param stats.only a \code{logical} value that indicates whether only the statistical outcomes are returned
#' 
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import broom
#' 
#' @export

compare_ocr <- function(df, robust = TRUE, stats.only = FALSE) {
  
  # calculate APR
  ocr_df <- summarize_ocr(df)
  
  n_groups <- length(unique(ocr_df$Group))
  
  ocr_nest <- ocr_df %>% 
    gather(what, val, -Group, -Well) %>% 
    filter(Group != "Background") %>% 
    group_by(what) %>% 
    nest()
  
  if (robust == TRUE) {
    if (n_groups >= 3) {
      ocr_res <- ocr_nest %>% 
        mutate(stat = map(data, ~tidy(kruskal.test(val~Group, data = .))))
    } else {
      ocr_res <- ocr_nest %>% 
        mutate(stat = map(data, ~tidy(wilcox.test(val~Group, data = .))))
    }
  } else if (robust == FALSE) {
    if (n_groups >= 3) {
      ocr_res <- ocr_nest %>% 
        mutate(stat = map(data, ~tidy(aov(val~Group, data = .)))) %>% 
        filter(term == "Group")
    } else {
      ocr_res <- ocr_nest %>% 
        mutate(stat = map(data, ~tidy(t.test(val~Group, data = ., paired = FALSE))))
    }
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
