format.msd <- function(x){
  paste("M = ",mean(x, na.rm=T) %>% round(2), ", SD = ",sd(x, na.rm=T) %>% round(2), sep="")
}
