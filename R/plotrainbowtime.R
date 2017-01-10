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
           year %in% c("2006", "2009", "2012", "2015"),
           cnt %in% cnts) %>%
  mutate(label = giveLabel(subject, cnt_lab, isco_lab,
			   ave.perf, se, pop.share)) -> sdf

  ggplot(sdf, aes(x = year, y = ave.perf, color = isco, group = isco_lab)) +
    geom_line(size = 1) + 
    geom_point_interactive(aes(tooltip = label), size = 1.5) +
    theme_tufte(base_size = 10) +
    theme(#panel.grid.major.y = element_line(linetype = 2, size = 0.5, color = "grey70"),
	  panel.grid = element_line(linetype = 2, size = 0.5, color = "grey70")) +
    scale_color_manual(values = colors, guide = "none") +
    xlab("Year") +
    ylab("Mean performance") +
    facet_wrap(~cnt_lab)
}

