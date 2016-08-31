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


#' Duplicate rows to count each student two times - one for each parent's occupation - while calculating ave perf.
#'
#' @inheritParams isco_cat
#' @param nc String character of the form "1"/"2" indicating if primary or secondary isco categories are to be added.
#'        "1" corresponds to primary categories.
#'
#' @return Data frame given in data argument with twice as much rows and added column
#'         with both parents' isco codes.
#' @export

add_isco <- function(data, isco_cols, nc = "1") {
    expr <- paste0("as.factor(substr(i, ", nc, ", ", nc, "))")
    if(nc == "2")
        expr <- paste0("as.factor(substr(i, ", nc, ", ", nc, "))")
    data %>%
        mutate_(mother_occu = interp(expr, i = as.name(isco_cols[1])),
                father_occu = interp(expr, i = as.name(isco_cols[2]))) %>%
        gather(isco_cat_name, isco_cat, mother_occu, father_occu)
}
