table.spss <- function(x) {
  cbind( freq = table(x),
         perc = 100*table(x) / sum(table(x)),
         cum.perc = cumsum(100*table(x) / sum(table(x)))
  )
}
