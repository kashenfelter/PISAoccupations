#' Plot of changes in average performances in time for one or two countries.
#'
#' @param csubject Character of the from "MATH"/"READ"/"SCIE".
#' @param cnts Country codes of countries to compare on a rainbow plot.
#' @param disp Logical vector - first element indicates, if standard errors are to be displayed,
#'        second argument indicates, if trend lines are to be displayed.
#' @param isco_cats ISCO categories to plot - as a character - numbers of categories.
#'
#' @return GGplot2 object.
#'
#' @export

plot_time <- function(csubject, cnts, disp, isco_cats = as.character(1:9)) {
    pisa %>%
        filter(subject == csubject,
               cnt %in% cnts,
               isco %in% c(isco_cats)) %>%
    ggplot(aes(x = year, y = ave.perf, shape = cnt_lab,
               color = isco, group = as.factor(paste0(isco, cnt))), linetype = 2) +
        geom_point(size = 4) +
        theme_bw(base_size = 20) +
        theme(legend.position = "right",
              axis.text.x = element_text(angle = 90)) +
#         scale_shape_discrete(name = "Country") +
        scale_shape_manual(name = "Country", values = c(1,4)) +
        # sort(substr(cnts[1],1,1), substr(cnts[2],1,1)) - żeby dać literki na wykres
        scale_color_discrete(guide = "none") +
        xlab("") +
        ylab("") +
        facet_grid(~isco, labeller = as_labeller(naming[naming != "Country"])) +
        theme(panel.grid.major.y = element_line(linetype = 2, size = 0.5, color = "black")) -> plt

    if(disp[1] & disp[2])
        plt <- plt + geom_pointrange((aes(ymin = ave.perf - se, ymax = ave.perf + se)), linetype = 2) +
            geom_smooth(aes(linetype = cnt, alpha = cnt), method = "lm", se = F, size = 1.5, show.legend = F)
    else if(disp[1] & !disp[2])
        plt <- plt + geom_pointrange((aes(ymin = ave.perf - se, ymax = ave.perf + se)), linetype = 2)
    else if(!disp[1] & disp[2])
        plt <- plt + geom_smooth(aes(linetype = c, alpha = cnt), method = "lm", se = F, size = 1.5, show.legend = F)

    return(plt)
}
