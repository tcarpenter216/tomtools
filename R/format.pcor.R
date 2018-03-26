format.pcor <- function(temp){
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



  p <- aparound(temp$p.value)
  df <- temp$n-3


  final <- paste("r(",df,") = ",r,", p ",p, sep="")
  return(final)
}
