### Input
# Five inputs: NSim: the number of simulations 
#              N_First: the sample size of the first batch (35 in our example)   
#              N_2: the additional sample size if more data is collected
#              qLower: the lower bound of the first batch z statistic needed to collect more data
#                 in our example, if z1 < 1, Sepien will give up and file the results away, so qLower=1
#              qUpper: the lower bound of the first batch z statistic needed to publish
#                 if z1 > 1.644, Sepien will get to publish
### Output
#     zVec: a numeric vector of z scores from the third branch
#       entries are only non-NA if Sepien decides to collect more data
#
### Details
#     the calculations follow the instructions in the blog post


Gen_Sepien_Dist <- function(NSim,N_First,N_2,qLower,qUpper) {
  zVec <- rep(NA,times=NSim)
  for (i in 1:NSim) {
    
    firstX <- rnorm(n=N_First,mean=0,sd=1)
    firstMean <- mean(firstX)
    firstZ <- sqrt(N_First) * firstMean
    
    # if p-value for one-sided test was 0.15 or greater, consider collecting more data
    # how important is honesty at this point? PRETTY IMPORTANT I THINK

    
    if ((firstZ >= qLower) & (firstZ < qUpper)) {
      secondX <- rnorm(n=N_2,mean=0,sd=1)
      fullMean <- mean(c(firstX,secondX))
      zVec[i] <- fullMean * sqrt(N_First+N_2)
    } 

  }  
  return(zVec)
}  