#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param subject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param cyear Chosen year as a character - 2003/2006/2009/2012.
#'
#' @return GGplot2 object.
#'
#' @export

plot_rainbow <- function(csubject, cnts, cyear) {
    pisa %>%
        filter(subject == csubject,
               year == cyear,
               cnt %in% cnts) %>%
        mutate(no = ifelse(cnt == cnts[1], 1, 3),
               isco2 = ifelse(isco == "cnt", cnt_lab, isco_lab)) -> sdf

    ggplot(subset(sdf, isco != "cnt"), aes(x = no, y = ave.perf, color = isco, group = isco2, label = isco2)) +
        theme_tufte(base_size = 18) +
        geom_point(data = subset(sdf, isco != "cnt"), aes(size = pop.share)) +
        geom_line(size = 1.5) +
        geom_text(data = subset(sdf, isco != "cnt" & no == 1), hjust = "outward", size = 8, nudge_x = -0.1) +
        geom_text(data = subset(sdf, isco != "cnt" & no == 3), hjust = "outward", size = 8, nudge_x = 0.1) +
        geom_line(data = subset(sdf, isco == "cnt"), aes(group = year), size = 1.5, color = "black") +
        geom_point(data = subset(sdf, isco == "cnt"), color = "black") +
        geom_text(data = subset(sdf, isco == "cnt" & no == 1),
                  color = "black", hjust = "outward", size = 8, nudge_x = -0.1) +
        geom_text(data = subset(sdf, isco == "cnt" & no == 3),
                  color = "black", hjust = "outward", size = 8, nudge_x = 0.1) +
        scale_size(guide = "none") +
        scale_color_discrete(guide = "none") +
        theme(axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none") +
        xlab("") +
        ylab("") +
        xlim(-4, 8)
}
