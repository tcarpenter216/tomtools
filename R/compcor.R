compcor <- function(outcome, pred1, pred2, text=FALSE){
  require(cocor)
  # raw values before missing data drop
  raw <- data.frame(o = outcome, pred1 = pred1, pred2=pred2)
  cor1.raw <- cor.test(raw$o, raw$pred1)
  cor2.raw <- cor.test(raw$o, raw$pred2)
  diff <- cor1.raw$estimate-cor2.raw$estimate

  # compare dependent corrs
  temp <- data.frame(o = outcome, pred1 = pred1, pred2=pred2)
  temp <- temp[complete.cases(temp),]
  cor1 <- cor.test(temp$o, temp$pred1)
  cor2 <- cor.test(temp$o, temp$pred2)
  backend <- cor(temp$pred1, temp$pred2)
  temp2 <-cocor::cocor.dep.groups.overlap(cor1$estimate, cor2$estimate, backend, nrow(temp))
  z <- attributes(temp2)$hittner2003$statistic
  p <- attributes(temp2)$hittner2003$p.value
  diff <- cor1$estimate-cor2$estimate

  if(text==TRUE){return(paste("Dr = ", round(diff, 2), ", z = ", round(z, 2), ", p = ", format.p(p), sep=""))}
  if(text==FALSE){return(data.frame(n=nrow(temp), r.pred1=cor1$estimate, r.pred1.p=cor1$p.value, r.pred2=cor2$estimate, r.pred2.p=cor2$p.value, diff=diff, z=z, p=p))}
}
