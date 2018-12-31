# r function that displays model fit from an Mplus object


fit.print <- function(x){

  sums <- x$summaries

  pval.round <- function(p){
    if (p < .001) {p <- "< .001"}
    if (p >= .001 & p < .005) {p <- paste("= ",substr(as.character(round(p, 3)), 2, 5), sep="")}
    if (p >= .005) {p <- paste("= ",substr(as.character(round(p, 2)), 2, 4), sep="")}
    return(p)
  }

  round.twodecimal <- function(stat){
    stat <- round(stat, 2)
    dec.location <- str_locate(stat, "\\.")[1]
    if(is.na(dec.location)){
      stat <- paste(as.character(stat), ".00", sep="")
      return(stat)
    }
    len <- str_length(stat)
    num.dec <- len-dec.location
    if(num.dec==1) {stat <- paste(as.character(stat), "0", sep="")}
    stat <- as.character(stat)
    return(stat)
  }

  out <- paste("X2(",sums$ChiSqM_DF,") = ",round.twodecimal(sums$ChiSqM_Value), ", p ", pval.round(sums$ChiSqBaseline_PValue), ", CFI = ",
               round.twodecimal(sums$CFI), ", TLI = ", round.twodecimal(sums$TLI), ", RMSEA = ", round.twodecimal(sums$RMSEA_Estimate, 2),
               ", SRMR = ", round.twodecimal(sums$SRMR), sep="")

  return(out)
}
