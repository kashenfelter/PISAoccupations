#' Mean calculated from five plausible values.
#'
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances computed
#'                  for each plausible value.
#'
#' @return Vector of student's average performances.
#'

mean_o <- function(means) {
    means %>%
        select(starts_with("mpv")) %>%
        rowSums(., na.rm = T)/5
}
