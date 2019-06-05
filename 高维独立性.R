rm(list = ls())
setwd('E:/数统研究生/属性数据分析/data')
cnum <- c(9,6,6,8,297,377,47,81,11,8,4,7,446,613,57,92,13,9,3,7,511,826,59,116)
name <- list(c('上证','深证'),c('ST','非ST'),c('涨','跌'),c('卖出','观望','买入'))
names(name) <- c('交易所','ST','涨跌','评级')
a <- array(cnum,c(2,2,2,3),name)
#write.csv(a,'a.csv')
#######(A,B,C,D)
JY4 <- function(data){
  data.sum <- 0
  for(i in 1:2){
    for(j in 1:2){
      for(k in 1:2){
        for(l in 1:3){
          tmp <- data[i,j,k,l]*log(sum(data[i,,,])*sum(data[,j,,])*sum(data[,,k,])*sum(data[,,,l])/(data[i,j,k,l]*(sum(data))^3))
          data.sum <- sum(data.sum,tmp)
        }
        
      }
      
    }
  }
  data.chisq <- -2*data.sum
  return(data.chisq)
}

a4.chisq <- JY4(a)
chi2.4 <- qchisq(0.05,18,lower.tail = F)

#######(AB,AC,AD)
JY3 <- function(data){
  data.sum <- 0
  for(j in 1:2){
    for(k in 1:2){
      for(l in 1:3){
          tmp <- data[1,j,k,l]*log(sum(data[1,j,,])*sum(data[1,,k,])*sum(data[1,,,l])/(data[1,j,k,l]*(sum(data[1,,,]))^2))
          data.sum <- sum(data.sum,tmp)
        }
        
      }
      
    }
  data.chisq <- -2*data.sum
  return(data.chisq)
}

a3.chisq <- JY3(a)
chi2.3 <- qchisq(0.05,11,lower.tail = F)

JY3.2 <- function(data){
  data.sum <- 0
  for(i in 1:2){
    for(k in 1:2){
      for(l in 1:3){
        tmp <- data[i,2,k,l]*log(sum(data[i,2,,])*sum(data[,2,k,])*sum(data[,2,,l])/(data[i,2,k,l]*(sum(data[,2,,]))^2))
        data.sum <- sum(data.sum,tmp)
      }
      
    }
    
  }
  data.chisq <- -2*data.sum
  return(data.chisq)
}

a3.chisq <- JY3.2(a)
chi2.3 <- qchisq(0.05,11,lower.tail = F)

JY3.3 <- function(data){
  data.sum <- 0
  for(i in 1:2){
    for(j in 1:2){
      for(l in 1:3){
        tmp <- data[i,j,2,l]*log(sum(data[i,,2,])*sum(data[,j,2,])*sum(data[,,2,l])/(data[i,j,2,l]*(sum(data[,,2,]))^2))
        data.sum <- sum(data.sum,tmp)
      }
      
    }
    
  }
  data.chisq <- -2*data.sum
  return(data.chisq)
}

a3.chisq <- JY3.3(a)
chi2.3 <- qchisq(0.05,11,lower.tail = F)

JY3.4 <- function(data){
  data.sum <- 0
  for(i in 1:2){
    for(j in 1:2){
      for(k in 1:2){
        tmp <- data[i,j,k,3]*log(sum(data[i,,,3])*sum(data[,j,,3])*sum(data[,,k,3])/(data[i,j,k,3]*(sum(data[,,,3]))^2))
        data.sum <- sum(data.sum,tmp)
      }
      
    }
    
  }
  data.chisq <- -2*data.sum
  return(data.chisq)
}

a3.chisq <- JY3.4(a)
chi2.3 <- qchisq(0.05,11,lower.tail = F)
