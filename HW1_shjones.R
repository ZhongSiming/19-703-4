# Homework 1 write a simulation in R that replicates Situation B from 
# Simmons et al. (2011). Results for Situation B were obtained by conducting 
# one t test after collecting 20 observations per cell and another after 
# collecting an additional 10 observations per cell.


SituationB <- function(pval){
  #Group 1: Generate all 30 observations at once
  Liking1 <- rnorm(30, 0, 1)
  # Group 2
  Liking2 <- rnorm(30, 0, 1)
  # For the first t-test, use the first 20 observations
  t.test1 <- t.test(Liking1[1:20], Liking2[1:20], var.equal = TRUE)
  # For the second t-test, use all 30
  t.test2 <- t.test(Liking1[1:30], Liking2[1:30], var.equal = TRUE)
  #signif = 1 if at least one test gives p < pval, 0 otherwise
  signif <- ifelse(t.test1$p.value < pval | #Vertical bar is logical "or"
                     t.test2$p.value < pval,
                   1, 0)
  return(signif) #value returned by the function
}

  replicates <- replicate(15000, SituationB(.05))
  SitBSim <- mean(replicates)
  SitBSim