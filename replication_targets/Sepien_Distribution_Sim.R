Sepien_Distribution_Sim <- function() {
  set.seed(7788)
  
  # define simulation parameters
  N2Vals <- c(16, 100, 175, 500,1000,10000)
  qLower <- 1
  qUpper <- qnorm(0.95)
  NSim <- 100000 # we need a lot of simulations to estimate tail probabilities
  N_First <- 35
  
  
  sepienDistList <- list()
  for (tempN2 in N2Vals) {
    tempZDist <- Gen_Sepien_Dist(NSim,N_First,tempN2,qLower,qUpper)
    sepienDistList[[sprintf("N2 = %d",tempN2)]] <- tempZDist
  }
  
  saveRDS(sepienDistList,file = "data/sepienDistList.RDS")  
  return(sepienDistList)
}
