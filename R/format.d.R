format.d <- function(temp){

  d <- round(temp$estimate, 2)


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

  CI <- temp$conf.int
  ci.1 <- ciround(CI[1])
  ci.2 <- ciround(CI[2])
  output <- paste("d = ",d,", 95% CI [", ci.1, ", ",ci.2,"]",sep="")
  return(output)
}
