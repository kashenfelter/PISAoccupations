#' Extract first and first two digits of ISCO code as factors both for mother's and father's occupation.
#'
#' @param isco_cols Names of columns with mother's and father's occupation codes (in this order).
#' @param data Name of a data frame containing columns given in previous arguments.
#'
#' @return Data frame given in 'data' argument with four described columns added.
#'

isco_cat <- function(isco_cols, data) {
    data %>%
        mutate_(mother_occuI = interp("as.factor(substr(i, 1, 1))", i = as.name(isco_cols[1])),
                mother_occuII = interp("as.factor(substr(i, 1, 2))", i = as.name(isco_cols[1])),
                father_occuI = interp("as.factor(substr(i, 1, 1))", i = as.name(isco_cols[2])),
                father_occuII = interp("as.factor(substr(i, 1, 2))", i = as.name(isco_cols[2])))
}
