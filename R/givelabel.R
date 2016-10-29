#' Labels for shiny app's tooltips.
#'
#' @param subj Column with the subject name.
#' @param cnt Column with the country label.
#' @param isco Column with the isco label.
#' @param avep Column with students' average performance.
#' @param se Column with standard errors.
#' @param pop Column with population share.
#'
#' @return html object for a tooltip.
#'

giveLabel <- function(subj, cnt, isco, avep, se, pop) {
    return(paste(paste("Subject:", subj), 
		 cnt, 
		 isco,
		 paste("Mean (plausible values):", round(avep, 2)), 
		 paste("Standard error:", round(se, 2)), 
		 paste("Population share:", paste0(round(pop/1000), "k")),
		 sep = "<br />"))
}
