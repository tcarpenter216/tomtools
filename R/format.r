format.r <- function(temp){
  r <- temp$estimate
  if (r<0) {r.NEG <- TRUE} else {r.NEG <- FALSE}
  r <- as.character(round(abs(r), 2))
  if (substr(r, 1, 1)=="0") {r <- substr(r, 2, 4)} else {r <- paste("-",substr(r, 3, 4), sep="")}
  if(r.NEG==TRUE) {r <- paste("-",r, sep="")}

  aparound <- function(p){
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste("= ",substr(as.character(round(p, 3)), 2, 5), sep="")}
    if (p >= .005) {p <- paste("= ",substr(as.character(round(p, 2)), 2, 4), sep="")}
    return(p)
  }

  ciround <- function(p){
    if (p < 0) {neg <- TRUE} else {neg <- FALSE}
    p <- abs(p)
    p.num <- p
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- substr(as.character(round(p, 3)), 2, 5)}
    if (p >= .005) {p <- substr(as.character(round(p, 2)), 2, 4)}
    if (neg == TRUE & p.num >= .001){ p <- paste("-",p, sep="")}
    if (neg == TRUE & p.num < .001){
      p <- paste("> -", substr(p, 3,nchar(p)), sep="")
      }
    return(p)
  }

  p <- aparound(temp$p.value)
  df <- temp$parameter
  CI <- temp$conf.int
  ci.1 <- ciround(CI[1])
  ci.2 <- ciround(CI[2])

  final <- paste("r(",df,") = ",r,", p ",p,", 95% CI [",ci.1,", ", ci.2,"]", sep="")
  return(final)
}
