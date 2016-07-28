#' Labels for shiny app's tooltips.
#'
#' @param point Object from a ggvis plot.
#'
#' @return html object for a tooltip.
#'

give_label <- function(point) {
    src <- as.list(pisa[pisa$id == point$id, ])
    return(paste(src$cnt_lab, "<br />",
          src$isco_lab, "<br />",
          "Mean:", round(src$ave.perf, 0), "<br />",
          "Standard error:", round(src$se, 2), "<br />",
          "Population share:", round(src$pop.share, 2)))
}
