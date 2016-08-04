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
    if("AUT" %in% cnts & cyear == "2012") {
        ggplot(pisa) +
            geom_blank() +
            ggtitle("Austria did not provide data on parents' occupations") +
            theme_tufte(base_size = 16)
    }
    else {
        pisa %>%
            filter(subject == csubject,
                   year == cyear,
                   cnt %in% cnts) %>%
            mutate(no = ifelse(cnt == cnts[1], 1, 3),
                   isco2 = ifelse(isco == "cnt", cnt_lab, isco_lab),
                   text_pos = ave.perf) %>%
            arrange(cnt, ave.perf)-> sdf
        sdf$text_pos[sdf$no == 1] <- nice_text(sdf$text_pos[sdf$no == 1])
        sdf$text_pos[sdf$no == 3] <- nice_text(sdf$text_pos[sdf$no == 3])
        sdf$isco2[sdf$isco != "cnt"] <- apply(data.frame(sdf$isco[sdf$isco != "cnt"]), 1,
                                              {function(x) return(naming2[names(naming2) == x])})

        ggplot(subset(sdf, isco != "cnt"), aes(x = no, y = ave.perf, color = isco, group = isco2, label = isco2)) +
            theme_tufte(base_size = 16) +
            geom_point(data = subset(sdf, isco != "cnt"), aes(size = pop.share)) +
            geom_line(size = 1.5) +
            geom_text(data = subset(sdf, isco != "cnt" & no == 1), aes(y = text_pos), hjust = "outward", size = 7, nudge_x = -0.1) +
            geom_text(data = subset(sdf, isco != "cnt" & no == 3), aes(y = text_pos), hjust = "outward", size = 7, nudge_x = 0.1) +
            geom_line(data = subset(sdf, isco == "cnt"), aes(group = year), size = 1.5, color = "black") +
            geom_point(data = subset(sdf, isco == "cnt"), color = "black") +
            geom_text(data = subset(sdf, isco == "cnt" & no == 1),
                      color = "black", hjust = "outward", size = 7, nudge_x = -0.1) +
            geom_text(data = subset(sdf, isco == "cnt" & no == 3),
                      color = "black", hjust = "outward", size = 7, nudge_x = 0.1) +
            scale_size(guide = "none") +
            scale_color_discrete(guide = "none") +
            theme(axis.ticks.x = element_blank(),
                  axis.text.x = element_blank(),
                  legend.position = "none",
                  panel.grid.major.y = element_line(linetype = 2, size = 0.5, color = "grey"),
                  panel.grid = element_line(linetype = 2, size = 0.5, color = "grey"),
                  axis.text = element_text(face = "bold")) +
            xlab("") +
            ylab("Mean performance") +
            xlim(-4, 8)
    }
}
