qqplot <- function(mod){
  qqnorm(mod$residuals);  qqline(as.vector(mod$residuals))
}

fitplot <- function(mod){
  plot(fitted(mod), sqrt(abs(residuals(mod))));abline(lm(sqrt(abs(residuals(mod)))~fitted(mod)))

}
