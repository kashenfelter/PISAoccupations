#' Dot plot of average performances of all countries in given year.
#'
#' @param csubject Chosen subject - character "MATH"/"READ"/"SCIE"
#' @param cyear Chosen year of PISA study.
#' @param isco_cats Chosen isco categories.
#'
#' @return ggplot2 plot.
#'
#' @export
#'

plotDot <- function(csubject, cyear, isco_cats) {
  pisa %>%
    filter(year == cyear,
	   subject == csubject,
	   isco %in% isco_cats) %>%
    mutate(label = giveLabel(subject, cnt_lab, isco_lab, ave.perf, se, pop.share))-> sdf

  ggplot(sdf, aes(x = reorder(cnt_lab, cnt_avg), y = ave.perf, color = isco, size = pop.share)) +
    geom_point_interactive(aes(tooltip = label), size = 1.5) +
    ylab("Mean performance") +
    xlab("") +
    scale_color_manual(values = colors) +
    guides(color = "none",
	   size = "none") +
    coord_flip() +
    theme_tufte(base_size = 10)
}

