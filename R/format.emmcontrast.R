format.emmcontrast <- function(x){
  require(emmeans)
  #use for contrast() objects from emmeans
  pval.round <- function(p){
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste0("= ",substr(format(round(p, 3), nsmall=3), 2, 5))}
    if (p >= .005) {p <- paste0("= ",substr(format(round(p, 2), nsmall=2), 2, 4))}
    return(p)
  }

  round.twodecimal <- function(stat){format(round(stat, 2), nsmall=2)}

  out <- paste0("t(", x$df, ") = ", round.twodecimal(x$statistic), ", p ", pval.round(x$p.value))
  return(out)

}
