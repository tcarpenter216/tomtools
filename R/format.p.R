format.p <- function(x){
  require(tidyverse)
  x.char <- as.character(x)

  x.char[x < .001] <- "< .001"
  x.char

  threedec <- x < .005 & x >= .001
  x.char[threedec] <- x[threedec] %>% round(., 3) %>% as.character() %>% substr(., 2, nchar(.))

  twodec <- x > .005

  x.char[twodec] <- x[twodec] %>% round(., 2) %>% as.character() %>% substr(., 2, nchar(.))

  return(x.char)
}
