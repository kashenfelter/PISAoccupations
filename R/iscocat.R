#' Extract first and first two digits of ISCO code as factors both for mother's and father's occupation.
#'
#' @param data Name of a data frame containing columns given in isco_cols argument.
#' @param isco_cols Names of columns with mother's and father's occupation codes in this order.
#'
#' @return Data frame given in 'data' argument with four additional columns for mother's and father's
#'         primary and secondary ISCO categories.
#'

isco_cat <- function(data, isco_cols) {
    data %>%
        mutate_(mother_occuI = interp("as.factor(substr(i, 1, 1))", i = as.name(isco_cols[1])),
                mother_occuII = interp("as.factor(substr(i, 1, 2))", i = as.name(isco_cols[1])),
                father_occuI = interp("as.factor(substr(i, 1, 1))", i = as.name(isco_cols[2])),
                father_occuII = interp("as.factor(substr(i, 1, 2))", i = as.name(isco_cols[2])))
}
