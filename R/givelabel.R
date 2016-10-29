#' Labels for shiny app's tooltips.
#'
#' @param point Object from a ggvis plot.
#'
#' @return html object for a tooltip.
#'

giveLabel <- function(point) {
    src <- as.list(pisa[pisa$id == point$id, ])
    return(paste(paste("Subject:", src$subject), 
		 src$cnt_lab, 
		 src$isco_lab,
		 paste("Mean (plausible values):", round(src$ave.perf, 2)), 
		 paste("Standard error:", round(src$se, 2)), 
		 paste("Population share:", paste0(round(src$pop.share/1000), "k")),
		 sep = "<br />"))
}
