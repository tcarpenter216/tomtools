format.aov <- function(x){
  require(broom)
  require(sjstats)

  pval.round <- function(p){
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste0("= ",substr(format(round(p, 3), nsmall=3), 2, 5))}
    if (p >= .005) {p <- paste0("= ",substr(format(round(p, 2), nsmall=2), 2, 4))}
    return(p)
  }

  round.twodecimal <- function(stat){format(round(stat, 2), nsmall=2)}

  out <- broom::tidy(x)
  out <- paste0("F(", out$df[2], ", ",out$df[3],") = ", round.twodecimal(out$statistic[2]), ", p ", pval.round(out$p.value[2]), ", w2 = ", round.twodecimal(sjstats::omega_sq(x)[2,2]))
  return(out)
}
