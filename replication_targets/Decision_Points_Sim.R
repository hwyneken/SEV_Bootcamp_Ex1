Decision_Points_Sim <- function(sepienDistList) {
  set.seed(7788) # don't change this seed 
  
  trueMu <- 0.2
  trueSD <- 1
  N_First <- 35
  
  sigLevel <- 0.05
  
  x <- rnorm(n=N_First,mean=trueMu,sd=trueSD)
  xbar_1 <- mean(x) # 0.23
  
  ### try one example n2=16
  set.seed(2345) # don't change this seed
  x2_16 <- rnorm(n=16,mean=trueMu,sd=trueSD)
  fullX_16 <- c(x,x2_16)
  zFull_16 <- sqrt(35 + 16) * mean(fullX_16) # 1.648
  
  NSim <- 1000
  
  ### SLS - what are the Type I and Type II error rates?
  refDist_Sepien <- sepienDistList[["N2 = 16"]]
  critVal_16 <- quantile(refDist_Sepien,probs = 0.95,na.rm=TRUE)
  
  # type I error: assume that mu = 0 - use the existing xbar_1
  zVec_16_Null <- rep(NA,NSim)
  for (i in 1:NSim) {
    x1 <- x
    x2 <- rnorm(n=16,mean=0,sd=1)
    fullX <- c(x1,x2)
    zVec_16_Null[i] <- sqrt(35 + 16) * mean(fullX)
  }
  
  # type II error: assume that mu = xbar_1, use the existing xbar_1
  zVec_16_POW <- rep(NA,NSim)
  for (i in 1:NSim) {
    x1 <- x
    x2 <- rnorm(n=16,mean=xbar_1,sd=1)
    fullX <- c(x1,x2)
    zVec_16_POW[i] <- sqrt(35 + 16) * mean(fullX)
  }
  
  ### N2=100 - what are the Type I and Type II error rates?
  refDist_Sepien <- sepienDistList[["N2 = 100"]]
  critVal_100 <- quantile(refDist_Sepien,probs = 0.95,na.rm=TRUE)
  
  # type I error: assume that mu = 0 - use the existing xbar_1
  zVec_100_Null <- rep(NA,NSim)
  for (i in 1:NSim) {
    x1 <- x
    x2 <- rnorm(n=100,mean=0,sd=1)
    fullX <- c(x1,x2)
    zVec_100_Null[i] <- sqrt(35 + 100) * mean(fullX)
  }
  
  # type II error: assume that mu = xbar_1, use the existing xbar_1
  zVec_100_POW <- rep(NA,NSim)
  for (i in 1:NSim) {
    x1 <- x
    x2 <- rnorm(n=100,mean=xbar_1,sd=1)
    fullX <- c(x1,x2)
    zVec_100_POW[i] <- sqrt(35 + 100) * mean(fullX)
  }
  
  
  ## N2 = 175, same analysis
  refDist_Sepien <- sepienDistList[["N2 = 175"]]
  critVal_175 <- quantile(refDist_Sepien,probs = 0.95,na.rm=TRUE)
  
  # type I error: assume that mu = 0 - use the existing xbar_1
  zVec_175_Null <- rep(NA,NSim)
  for (i in 1:NSim) {
    x1 <- x
    x2 <- rnorm(n=175,mean=0,sd=1)
    fullX <- c(x1,x2)
    zVec_175_Null[i] <- sqrt(35 + 175) * mean(fullX)
  }
  
  # type II error: assume that mu = xbar_1, use the existing xbar_1
  zVec_175_POW <- rep(NA,NSim)
  for (i in 1:NSim) {
    x1 <- x
    x2 <- rnorm(n=175,mean=xbar_1,sd=1)
    fullX <- c(x1,x2)
    zVec_175_POW[i] <- sqrt(35 + 175) * mean(fullX)
  }
  
  ### make gt table
  # https://gt.rstudio.com/
  # https://gt.rstudio.com/reference/gtsave.html
  require(gt)
  
  ChoiceVec <- c("N2 =  16","N2 = 100","N2 = 175")
  critValVec <- c(critVal_16,critVal_100,critVal_175)
  TypeIErrorVec <- c(mean(zVec_16_Null > critVal_16),
                     mean(zVec_100_Null > critVal_100),
                     mean(zVec_175_Null > critVal_175))
  PowerVec <- c(mean(zVec_16_POW > critVal_16),
                mean(zVec_100_POW > critVal_100),
                mean(zVec_175_POW > critVal_175))
  
  tableDF <- data.frame(N2_Strategy = ChoiceVec,
                        CritVal = round(critValVec,2),
                        TypeIError = round(TypeIErrorVec,2),
                        Power = round(PowerVec,2))
  colnames(tableDF) <- c("N2 Choice","Sepien Critical Value",
                         "Type I Error Rate","Power")
  
  table1 <- gt(data = tableDF)
  gtsave(table1,file = "images/resTable.png")  
}

