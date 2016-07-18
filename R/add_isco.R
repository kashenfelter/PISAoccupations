#' Duplicate rows to count each student two times - one for each parent's occupation - while calculating ave perf.
#'
#' @param isco_cols Names of columns with mother's and father's occupation codes - in this order.
#' @param data Name of a data frame containing columns given in previous arguments.
#'
#' @return Data frame given in data argument with twice as much rows and added column
#'         with both parents' isco codes.
#' @export

add_isco <- function(isco_cols, data) {
    data %>%
        mutate_(ID = 1:dim(data)[1],
                mother_occuI = interp("as.factor(substr(i, 1, 1))", i = as.name(isco_cols[1])),
                father_occuI = interp("as.factor(substr(i, 1, 1))", i = as.name(isco_cols[2]))) %>%
        melt(measure.vars = c("mother_occuI", "father_occuI"), variable.name = "isco_cat_name", value.name = "isco_cat")

}
