#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param subject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param cyear Chosen year as a character - 2003/2006/2009/2012.
#'
#' @return GGplot2 object.
#'
#' @export

plot_rainbow_shiny <- function(csubject, cnts, cyear) {
    pisa %>%
        filter(subject == csubject, year == cyear, cnt %in% cnts) %>%
        arrange(as.character(cnt), s.character(isco)) %>%
        mutate(no = if_else(cnt == sort(cnts)[1], 1, 5)) %>%
        mutate(isco2 = ifelse(isco == "cnt", as.character(cnt), "tba")) -> sdf
    sdf$isco2[sdf$isco != "cnt"] <- isco_text_plt[as.integer(as.character(sdf$isco[sdf$isco != "cnt"])) + 1]

        ggplot(sdf, aes(x = no, y = ave.perf, color = isco, group = isco2, label = isco2)) +
        theme_bw() +
        geom_line(size = 1.5) +
        geom_text_repel(data = subset(sdf, no == 1 & !(isco2 %in% cnts)), nudge_x = -1) +
        geom_text_repel(data = subset(sdf, no == 5 & !(isco2 %in% cnts)), nudge_x =  1) +
        geom_line(data = subset(sdf, isco2 %in% cnts), aes(group = year), size = 1.5, color = I("black")) +
        geom_point(data = subset(sdf, isco2 %in% cnts[1]), aes(size = pop.share), color = I("black")) +
        geom_point(data = subset(sdf, isco2 %in% cnts[2]), aes(size = pop.share), color = I("black")) +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.position = "none") +
        xlab("") +
        ylab("") +
        geom_text_repel(data = subset(sdf, isco2 %in% cnts), aes(x = no, y = ave.perf, label = isco2), color = I("black")) +
        xlim(-3, 9)
}
