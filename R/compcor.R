
compcor <- function(x, y1, y2){
  require(cocor)
  temp <- data.frame(x, y1, y2)
  temp <- temp[complete.cases(temp),]
  with.y1 <- cor(temp$x, temp$y1)
  with.y2 <- cor(temp$x, temp$y2)
  backend <- cor(temp$y1, temp$y2)
  diff <- with.y1-with.y2
  z <- cocor.dep.groups.overlap(with.y1, with.y2, backend, nrow(temp))@hittner2003$statistic
  p <- cocor.dep.groups.overlap(with.y1, with.y2, backend, nrow(temp))@hittner2003$p.value
  return(list(diff=diff, z=z, p=p))
  
}