#' Dot plot of average performances of all countries in given year.
#'
#' @param csubject Chosen subject - character "MATH"/"READ"/"SCIE"
#' @param cyear Chosen year of PISA study.
#'
#' @return ggplot2 plot.
#'
#' @export
#'

plotDot <- function(csubject, cyear) {
  pisa %>%
    filter(year == cyear,
	   subject == csubject) %>%
    mutate(label = giveLabel(subject, cnt_lab, isco_lab, ave.perf, se, pop.share))-> sdf

  ggplot(sdf, aes(x = reorder(cnt_lab, cnt_avg), y = ave.perf, color = isco, size = pop.share)) +
    geom_point_interactive(aes(tooltip = label), size = 2) +
    ylab("Mean performance") +
    xlab("") +
    scale_color_manual(values = colors) +
    guides(color = "none",
	   size = "none") +
    coord_flip() +
    theme_tufte()
}

ggiraph(code = print(plotDot("MATH", "2012")))
