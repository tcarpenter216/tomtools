format.p <- function(p){

    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste0("= ",substr(format(round(p, 3), nsmall=3), 2, 5))}
    if (p >= .005) {p <- paste0("= ",substr(format(round(p, 2), nsmall=2), 2, 4))}
    return(p)

}
