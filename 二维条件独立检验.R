setwd('E:/数统研究生/属性数据分析/data')
F1 <- read.csv('交易所.csv', header = F)
F2 <- read.csv('ST.csv', header = F)
F3 <- read.csv('涨幅.csv', header = F)
F.chisq <- function(F){
  F.sum <- 0
  for(i in 1:2) {
    for(j in 1:3){
      tmp <- (F[i,j])^2/(F[i,4]*F[3,j]/F[3,4])
      F.sum <- sum(F.sum,tmp)
    }
  }
  return(F.sum-F[3,4])
}
F1.chisq <- F.chisq(F1)
F2.chisq <- F.chisq(F2)
F3.chisq <- F.chisq(F3)
chi2 <- qchisq(0.05,2,lower.tail = F)
daf <- data.frame(F1.chisq,chi2)
daf[2,] <- c(F2.chisq,chi2)
daf[3,] <- c(F3.chisq,chi2)
write.csv(daf,'二维独立性检验结果.csv')

F.srb <- function(F){
  F.sum <- 0
  for(i in 1:2) {
    for(j in 1:3){
      tmp <- F[i,j]*log(F[i,4]*F[3,j]/(sum(F)*F[i,j]))
      F.sum <- sum(F.sum,tmp)
    }
  }
  return(-2*F.sum)
}
F1.srb <- F.srb(F1)
F2.srb <- F.srb(F2)
F3.srb <- F.srb(F3)
chi2.6 <- qchisq(0.05,6,lower.tail = F)
daf <- data.frame(F1.chisq,chi2)
daf[2,] <- c(F2.chisq,chi2)
daf[3,] <- c(F3.chisq,chi2)
write.csv(daf,'二维独立性检验结果.csv')
