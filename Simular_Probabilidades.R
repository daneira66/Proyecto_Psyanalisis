################################
# Simulación de probabilidades #
################################

prob1 <- function(n){
  probs <- c(runif(1,0,1))
  for(i in 2:n){
    probs[i] <- runif(1,0,1-sum(probs))
  }
  probs
}