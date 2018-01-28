# Homework 1 write a simulation in R that replicates Situation B from 
# Simmons et al. (2011). "Results for Situation B were obtained by conducting 
# one t test after collecting 20 observations per cell and another after 
# collecting an additional 10 observations per cell." (Simmons, et al 2011)

#This code is based on the code used for Situation A (Davis, 2018)
SituationB <- function(pval){
  # Group 1 generate 30
  Likers1 <- rnorm(30, 0, 1)
  # Group 2 generate 30
  Likers2 <- rnorm(30, 0, 1)
  # For the first t-test, use the first 20 observations
  t.test1 <- t.test(Likers1[1:20], Likers2[1:20], var.equal = TRUE)
  # For the second t-test, use all 30
  t.test2 <- t.test(Likers1[1:30], Likers2[1:30], var.equal = TRUE)
  # significant = 1 if at least one test gives p < pval, 0 otherwise
  significant <- ifelse(t.test1$p.value < pval | t.test2$p.value < pval, 1, 0)
  return(significant) #value returned by the function
}

replicates <- replicate(15000, SituationB(.05))
SitBMean <- mean(replicates)
SitBMean
