#' Changing distances between student's performances to make rainbow plot more readable.
#'
#' @param ave_perfs Vector of average performances filtered by plot_rainbow function.
#'
#' @return Modified vector in which distance between adjacent elements are greater then 8.
#'

nice_text <- function(ave_perfs) {
    nc <- 1 # Tylko po to, żeby wystartować pętlę.
    while(nc > 0) {
        nc <- 0
        for(i in 2:length(ave_perfs)) {
            if(ave_perfs[i] - ave_perfs[i - 1] >= 8)
                next
            else {
                ave_perfs[i] <- ave_perfs[i] + 1
                nc <- nc + 1
            } # else
        } # for
    } # while
    return(ave_perfs)
}
