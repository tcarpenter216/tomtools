format.msd <- function(x){
  paste("M = ", round(mean(x, na.rm=T), 2), ", SD = ",round(sd(x, na.rm=T), 2), sep="")
}
