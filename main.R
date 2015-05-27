
StartTime <- Sys.time()
options(digits = 15)
#source("UsingLib.R")
library(mvtnorm)
library(MASS)
library(QRM)
source("ReadingData.R")
source("myFun.R")

TM <- read.csv("TM.csv",header=T,sep=",",stringsAsFactors=F)
colN <- TM$TM
TM <- TM[,-1]
TM <- as.matrix(TM)
colnames(TM) <- colN
rownames(TM) <- colN
CumTMx <- t(apply(TM,1,cumsum))
#CumTMx <- CumTM(TM)
CumTMx[CumTMx>1] <- 0.99999
CumTMx[CumTMx==1] <- 0.99999
CumTMx[CumTMx<0.0001] <- 0.00001
StdTM <<- qnorm(CumTMx)


TSDataRNT <- returns(TSData)

CorMatrix <<- cor(TSDataRNT)
if(isPositiveDefinite(CorMatrix)==FALSE){
  xx <- makePositiveDefinite(CorMatrix)
  diag(xx) <- 1
  CorMatrix = xx
}


Rating <<- BondPort[,5]
Exposure <<- BondPort[,9]
LGD <<- BondPort[,10]

times <- 20

AnsArry <- rep(0,times)

#set.seed(5)

for( i in 1:times) {

num <- 5000

#mvRnd <- rmvnorm(num,mean=rep(0,nrow(CorMatrix)),CorMatrix,method="chol")

mvRnd1 <- mvrnorm(num, mu = rep(0,nrow(CorMatrix)), Sigma = CorMatrix,empirical = TRUE)

mvRnd <- qnorm(rcopula.gauss(num,Sigma=CorMatrix))

ratingStr <- t(apply(mvRnd,1,DefNDef,Rating))
#ratingNum <- apply(ratingStr,1,NumDef)
#ratingNum <- unlist(ratingNum)
PLArry <- t(apply(ratingStr,1,DefPL,Exposure,LGD))
PL <- apply(PLArry,1,sum)
CVaR <- quantile(PL,0.01)
CVaR <- CVaR[[1]]

AnsArry[i] <- CVaR

}

write.csv(AnsArry,"Output.csv")


EndTime <- Sys.time()
AllTime <- EndTime - StartTime
