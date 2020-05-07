#' ATP production rate comparison between groups
#' 
#' compare_apr compares APR between experimental groups
#' 
#' When there are two groups, the Wilcoxon signed-rank test will be used for the group comparison. 
#' When there are three groups, the Kruskal–Wallis one-way analysis of variance will be sued for the group comparison.
#' 
#' @param df a \code{data.frame} with raw data and produced by \code{import_seahorse}
#' @param which_assay a \code{character} which can be "XFp", "XFe24", "XF" or "XFe96"
#' @param stats.only a \code{logical} value that indicates whether only the statistical outcomes are returned
#' 
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import broom
#' 
#' @export

compare_apr <- function(df, which_assay = "XFe96", stats.only = FALSE) {
  
  # calculate APR
  apr_df <- summarize_apr(df, which_assay = which_assay)
  
  n_groups <- length(unique(apr_df$Group))
  
  apr_nest <- apr_df %>% 
    gather(what, val, -Group, -Well) %>% 
    filter(Group != "Background") %>% 
    group_by(what) %>% 
    nest()
  
  if (n_groups >= 3) {
    apr_res <- apr_nest %>% 
      mutate(stat = map(data, ~tidy(kruskal.test(val~Group, data = .))))
  } else {
    apr_res <- apr_nest %>% 
      mutate(stat = map(data, ~tidy(wilcox.test(val~Group, data = .))))
  }
  
  # output
  
  if (stats.only == TRUE) {
    apr_res %>% 
      select(-data) %>% 
      unnest(stat)
  } else {
    apr_res
  }
}
