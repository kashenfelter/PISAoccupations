#' Mean calculated from five plausible values.
#'
#' @param means_ppv Data frame returned by mean_ppvs function that contains average performances computed
#'                  for each plausible value.
#'
#' @return Vector of student's average performances.
#'

mean_o <- function(means_ppv) {
    return(rowSums(means_ppv[, paste0("mpv", 1:5)], na.rm  = T)/5)
}
