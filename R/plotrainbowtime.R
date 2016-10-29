#' Rainbow plot of student's average performance over years.
#'
#' @param sdf Data frame returned from reactive function.
#'
#' @return ggplot2 plot.
#'
#' @export
#'

plotRainbowTime <- function(csubject, cnts) {
  pisa %>%
    filter(subject == csubject,
	   cnt %in% cnts) -> sdf

  sdf %>%
    mutate(label = giveLabel(subject, cnt_lab, isco_lab, ave.perf, se, pop.share)) -> sdf

  ggplot(sdf, aes(x = year, y = ave.perf, color = isco_lab, group = isco_lab)) +
    geom_line(size = 2) + 
    geom_point_interactive(aes(tooltip = label), size = 3) +
    theme_tufte(base_size = 16) +
    theme(panel.grid.major.y = element_line(linetype = 2, size = 0.5, color = "grey"),
	  panel.grid = element_line(linetype = 2, size = 0.5, color = "grey")) +
    guides(color = "none") +
    xlab("Year") +
    ylab("Mean performance") +
    facet_wrap(~cnt)
}

