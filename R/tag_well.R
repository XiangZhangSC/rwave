#' Assign experiment group to a well
#'
#' @param experiment_setup a data.frame with two columns, group label and number of replicates
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @export
tag_well <- function(experiment_setup) {
    experiment_setup %>% 
        filter(Group != "Background") %>% 
        mutate(Tag = map2(Group, n, ~rep(.x, each = .y))) %>% 
        select(Tag) %>% 
        unnest(Tag) %>% 
        unlist() %>% 
        as.character()
}
