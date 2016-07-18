#' Rainbow plot of average performances for two countries grouped by isco categories designed for shiny app.
#'
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param cyear Chosen year as a character - 2003/2006/2009/2012.
#'
#' @return GGplot2 object.
#'
#' @export

plot_rainbow_shiny <- function(cnts, cyear) {
    math %>%
        filter(year == cyear, cnt %in% cnts) %>%
        arrange(as.character(cnt), as.numeric(as.character(isco))) %>%
        mutate(no = if_else(cnt == sort(cnts)[1], 1, 5)) %>%
        mutate(isco2 = ifelse(as.numeric(as.character(isco)) >= 0,
                              isco_text_plt[as.integer(as.character(isco))+1], as.character(cnt))) -> sdf
    sdf$isco2[is.na(sdf$isco2)] <- sort(cnts)
    ggplot(sdf, aes(x = no, y = ave.perf, color = isco, group = isco2, label = isco2)) +
        theme_bw() +
        geom_line(size = 1.5) +
        geom_text_repel(data = subset(sdf, no == 1 & !(isco2 %in% cnts))) +
        geom_text_repel(data = subset(sdf, no == 5 & !(isco2 %in% cnts))) +
        geom_line(data = subset(sdf, isco2 %in% cnts), aes(group = year), size = 1.5) +
        geom_point(data = subset(sdf, isco2 %in% cnts), aes(size = pop.share)) +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.position = "none") +
        xlab("") +
        ylab("") +
        geom_text_repel(data = subset(sdf, isco2 %in% cnts), aes(x = no, y = ave.perf, label = isco2), color = I("black")) +
        xlim(-3, 9)
}
