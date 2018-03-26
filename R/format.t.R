format.indt <- function(temp){

  t <- round(temp$statistic, 2)

  pval.round <- function(p){
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste("= ",substr(as.character(round(p, 3)), 2, 5), sep="")}
    if (p >= .005) {p <- paste("= ",substr(as.character(round(p, 2)), 2, 4), sep="")}
    return(p)
  }

  ciround <- function(ci){
    if (ci < 0) {neg <- TRUE} else {neg <- FALSE}
    ci <- abs(ci)
    ci.num <- ci
    if (ci < .001) {ci <- "< .001"}
    if (ci >= .001 & ci < .005) {ci <- round(ci, 3)}
    if (ci >= .005) {ci <- round(ci, 2)}
    if (neg == TRUE & ci.num >= .001){ ci <- paste("-",ci, sep="")}
    if (neg == TRUE & ci.num < .001){
      ci <- paste("> -", substr(ci, 3,nchar(p)), sep="")
    }
    return(ci)
  }

  p <- pval.round(temp$p.value)
  df <- round(temp$parameter, 2)
  CI <- temp$conf.int
  ci.1 <- ciround(CI[1])
  ci.2 <- ciround(CI[2])
  output <- paste("t(",df,") = ",t,", p ",p, ", 95% CI [", ci.1, ", ",ci.2,"]",sep="")
  return(output)
}
