#test for zero inflation in poisson data from van den Broek, J. (1995). A Score Test for Zero Inflation in a Poisson Distribution. Biometrics, 51(2), 738–743. https://doi.org/10.2307/2532959

zi.test <- function(data){
  data <- data[!is.na(data)]
  lambda_est <- mean(data)
  p0_tilde <- exp(-lambda_est)
  p0_tilde
  n0 <- sum(1*(!(data >0)))
  n <- length(data)

  # number of observtions 'expected' to be zero
  n*p0_tilde

  #now lets perform the JVDB score test
  numerator <- (n0 -n*p0_tilde)^2
  denominator <- n*p0_tilde*(1-p0_tilde) - n*lambda_est*(p0_tilde^2)

  test_stat <- numerator/denominator

  pvalue <- pchisq(test_stat,df=1, ncp=0, lower.tail=FALSE)
  pvalue

  return(list(exp.0 = n*p0_tilde, obs.0 = n0, chi.sq=test_stat, p.value=pvalue ))


}
