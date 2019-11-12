# r function that displays model fit from an Mplus object

fit.print <- function(x){

  sums <- x$summaries

  pval.round <- function(p){
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste0("= ",substr(format(round(p, 3), nsmall=3), 2, 5))}
    if (p >= .005) {p <- paste0("= ",substr(format(round(p, 2), nsmall=2), 2, 4))}
    return(p)
  }

  round.twodecimal <- function(stat){format(round(stat, 2), nsmall=2)}

  #RMSEA CI
  ll <- sums$RMSEA_90CI_LB
  ul <- sums$RMSEA_90CI_UB

  out <- paste0("X2(",sums$ChiSqM_DF,") = ", round.twodecimal(sums$ChiSqM_Value), ", p ", pval.round(sums$ChiSqM_PValue),
                ", SRMR = ", round.twodecimal(sums$SRMR),
                ", CFI = ",round.twodecimal(sums$CFI),
                # ", TLI = ", round.twodecimal(sums$TLI),
                ", RMSEA = ", round.twodecimal(sums$RMSEA_Estimate),
                ", RMSEA CI90 [", substring(format(round(ll, 3), nsmall=3), 2), ", ", substring(format(round(ul, 3), nsmall=3), 2),
                "]")

  return(out)
}
