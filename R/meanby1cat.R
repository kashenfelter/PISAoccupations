#' Average student's performances by country and one isco category.
#'
#' @param subject String of the form "MATH", "READ", "SCIE".
#' @param cnt_col Name of a column with country codes.
#' @param isco_cols Names of two columns with isco codes for mother's and father's occupation.
#' @param cat Character with a number of an isco category.
#' @param final_weight Name of a column that contains final student weights.
#'
#' @return Data frame with names of countries and means for a given isco category.
#'

mean_by_1_cat <- function(subject, cnt_col, isco_cols, cat, final_weights, data) {
    isco_cat(isco_cols, data) %>%
        filter(mother_occuI == cat | father_occuI == cat | mother_occuII == cat | father_occuII == cat) %>%
        mean_pv(subject, cnt_col, final_weights, .) %>% select(-population.share)
}
