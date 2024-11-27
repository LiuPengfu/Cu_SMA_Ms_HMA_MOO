## Probability of target range

fn.probability <- function(mean,sd,low,up){
  ## P{lower<X<upper} = Fai(upper)-Fai(lower)
  ## normalization
  ## (u-mu)/sig & (l-mu)/sig
  
  prob <- pnorm(q=up,mean=mean,sd=sd,lower.tail=T)-pnorm(q=low,mean=mean,sd=sd,lower.tail=T)
  return(prob)
  
}


fn.PI <- function(mean,sd,best){
  PI <-pnorm(q=best+0.01,mean=mean,sd=sd,lower.tail=F)
  return(PI)
}


fn.EI <- function(mean,sd,eps=0.01,max=T){
  
    if(max){
      EI <- sd*((mean-max(mean)-eps)/sd*pnorm((mean-max(mean)-eps)/sd) + dnorm((mean-max(mean)-eps)/sd))
    }else{
      EI <- sd*((min(mean)-mean-eps)/sd*pnorm((min(mean)-mean-eps)/sd) + dnorm((min(mean)-mean-eps)/sd))
    }
  
  return(EI)
}


fn.UCB <- function(mean,sd,k = 0.5){
  # if k>0 for Upper confidence bound
  # if k<0 for Lower confidence bound
  UCB <- mean + k*sd
  return(UCB)
}




fn.uncertainty <- function(sd1,sd2){
  
  uncertainty <- 4*sd1*sd2
  return(uncertainty)
  
}

fn.distance <- function(f1,f2,target){
  distance <- sqrt((f1-target[1])^2+(f2-target[2])^2)
  return(distance)
}

fn.disego <- function(f1,f2,sd1,sd2,target,property){
  dist.mean <- sqrt((f1-target[1])^2+(f2-target[2])^2)
  dist.sd <- sqrt(((f1-target[1])^2*sd1^2+(f2-target[2])^2*sd2^2)/((f1-target[1])^2+(f2-target[2])^2))
  disego <- fn.EI(mean = dist.mean, sd = dist.sd,property = property,max = F)
  return(disego)
}

fn.cand.disego <- function(pareto,f1,f2,sd1,sd2,target,property){
  pareto$disego <- fn.disego(f1,f2,sd1,sd2,target,property)
  candidate <- pareto[which(pareto$disego == max(pareto$disego)),]
  return(candidate)
}


fn.cand.uncertainty <- function(pareto,sd1,sd2){
  pareto$uncertainty <- fn.uncertainty(sd1,sd2)
  candidate <- pareto[which(pareto$uncertainty == max(pareto$uncertainty)),]
  return(candidate)
}


fn.cand.dist <- function(pareto,f1,f2,target){
  pareto$distance <- fn.distance(f1,f2,target)
  candidate <- pareto[which(pareto$distance == min(pareto$distance)),]
  return(candidate)
}

## SOME INDICATOR CALCULATION
calDis <- function(virtualData,finalTarget){
  
  # calculate the mean and sd for distance
  dis.mean <- sqrt((virtualData$pred1-finalTarget$f1)^2 + (virtualData$pred1-finalTarget$f1)^2)
  dis.sd <- sqrt(((virtualData$pred1-finalTarget$f1)^2 * virtualData$sd1^2 + 
                    (virtualData$pred2-finalTarget$f2)^2 * virtualData$sd2^2) /
                   ((virtualData$pred1-finalTarget$f1)^2 + (virtualData$pred2-finalTarget$f2)^2))
  virtualData$dis.mean <- dis.mean
  virtualData$dis.sd <- dis.sd 
  
  return(virtualData)
}

calArg <- function(virtualData,finalTarget,min=T){
  # finalTarget is a point named f1 f2
  
  pareto <- virtualData[paretoFront(virtualData$pred1,virtualData$pred2,min),]
  pareto$arg.mean <- (pareto$pred1*finalTarget$f1 + pareto$pred2*finalTarget$f2)/
    (sqrt(pareto$pred1^2+pareto$pred2^2)*sqrt(finalTarget$f1^2+finalTarget$f2^2))
  pareto$arg.sd <- (((finalTarget$f1*pareto$pred2^2-finalTarget$f2*pareto$pred1*pareto$pred2)^2*
                       pareto$sd1^2)+
                      ((finalTarget$f2*pareto$pred1^2-finalTarget$f1*pareto$pred1*pareto$pred2)^2*
                         pareto$sd2^2)) /
    ((pareto$pred1^2+pareto$pred2^2)^3 * (finalTarget$f1^2+finalTarget$f2^2))
  return(pareto)
}

calArea <- function(point,train,reference=c(10,10),min=T){
  # f1.mean <- paste(f1,'mean',sep = '.')
  # f2.mean <- paste(f2,'mean',sep = '.')
  point <- point[,1:5] # pareto front input
  newPareto <- rbind(train,point)
  newPareto <- newPareto[paretoFront(newPareto[,"pred1"],newPareto[,"pred2"],min),]
  newPareto <- newPareto[order(newPareto[,"pred1"]),]
  value <- 0
  if(min){
    if(dim(newPareto)[1] < 2){
      value <- (reference[1]-newPareto[1,"pred1"])*(reference[2]-newPareto[1,"pred2"])
    }else{
      for(i in 1:(dim(newPareto)[1]-1)){
        value <- value + (newPareto[i+1,"pred1"]-newPareto[i,f1.mean])*(reference[2]-newPareto[i,"pred2"])
      }
      value <- value + (reference[1]-newPareto[dim(newPareto)[1],"pred1"])*(reference[2]-newPareto[dim(newPareto)[1],"pred2"])
    }
  }else{
    value <- newPareto[1,"pred1"]*newPareto[1,"pred2"]
    if(dim(newPareto)[1] > 1){
      for(i in 2:(dim(newPareto)[1])){
        value <- value + (newPareto[i,"pred1"]-newPareto[i-1,"pred1"])*newPareto[i,"pred2"]
      }
    }
  }
  
  return(value) 
}

calEI <- function(prop,virtualData){
  z1 <- (virtualData$mean-max(prop$prop))/virtualData$sd
  ei.ego <- virtualData$sd*z1*pnorm(z1) + virtualData$sd*dnorm(z1)
  z2 <- -1*abs((virtualData$mean-max(max(virtualData$mean,max(prop$prop))))/virtualData$sd)
  ei.kg <- virtualData$sd*z2*pnorm(z2) + virtualData$sd*dnorm(z2)
  max.P <- pnorm(z1,mean = virtualData$mean,sd = virtualData$sd)
  ei <- data.frame("ei.ego"=ei.ego,"ei.kg"=ei.kg,"max.P"=max.P)
  return(ei)
}

calPI <- function(trainingData,virtualData,f1='f1',f2='f2',min=T){
  # f1.mean <- paste(f1,'mean',sep = '.')
  # f2.mean <- paste(f2,'mean',sep = '.')
  # f1.sd <- paste(f1,'sd',sep = '.')
  # f2.sd <- paste(f2,'sd',sep = '.')
  
  if(min){
    pareto <- trainingData[paretoFront(trainingData[,"f1"],trainingData[,"f2"],min),]
    pareto <- pareto[order(pareto[,"f1"]),] # current known pareto 
    f1.limit <- c(0,pareto[,"f1"],200) # 0 to each point to infinite
    f2.limit <- c(200,pareto[,"f2"])
    
    f1.mean <- virtualData[,"pred1"]
    f2.mean <- virtualData[,"pred2"]
    f1.sd <- virtualData[,"sd1"]
    f2.sd <- virtualData[,"sd2"]
    i <- 0
    n <- dim(virtualData)[1]-1
    repeat{
      i <- i+1
      j <- 0
      PI <- 0
      func <- fpdf(f1.mean[i],f2.mean[i],f1.sd[i],f2.sd[i])
      try({
        repeat{
          j <- j+1
          PI <- PI+dblquad(func,f1.limit[j],f1.limit[j+1],0,f2.limit[j]) # integrate by block to cover all non-domi region
          if(j>length(f1.limit)-2) break()
        }
      })
      virtualData$PI[i] <- PI
      if(i>(n)) break()
    }
  }else{
    pareto <- trainingData[paretoFront(trainingData[,"f1"],trainingData[,"f1"],min),]
    pareto <- pareto[order(pareto[,"f1"]),]
    f1.limit <- c(0,pareto[,"f1"],200)
    f2.limit <- c(pareto[,"f2"],0)
    
    f1.mean <- virtualData[,"pred1"]
    f2.mean <- virtualData[,"pred2"]
    f1.sd <- virtualData[,"sd1"]
    f2.sd <- virtualData[,"sd2"]
    i <- 0
    repeat{
      i <- i+1
      j <- 0
      PI <- 0
      func <- fpdf(f1.mean[i],f2.mean[i],f1.sd[i],f2.sd[i])
      try({
        repeat{
          j <- j+1
          PI <- PI+dblquad(func,f1.limit[j],f1.limit[j+1],f2.limit[j],200)
          if(j>length(f1.limit)-2) break()
        }
      })
      virtualData$PI[i] <- PI
      if(i>(dim(virtualData)[1]-1)) break()
    }
  }
  
  return(virtualData)
}

calCentroid <- function(trainingData,virtualData,min=T){
  # f1.mean <- paste(f1,'mean',sep = '.')
  # f2.mean <- paste(f2,'mean',sep = '.')
  # f1.sd <- paste(f1,'sd',sep = '.')
  # f2.sd <- paste(f2,'sd',sep = '.')
  if(dim(virtualData[-which(virtualData$PI==0),])[1]>0){
    # row of PI=0 in virtual > 0
    virtualData <- virtualData[-which(virtualData$PI==0),]
  }
  
  if(min){
    pareto <- trainingData[paretoFront(trainingData[,f1.mean],trainingData[,f2.mean],min),]
    pareto <- pareto[order(pareto[,f1.mean]),]
    f1.limit <- c(0,pareto[,f1.mean],200)
    f2.limit <- c(200,pareto[,f2.mean])
    
    i <- 0
    repeat{
      i <- i+1
      j <- 0
      PI.x <- 0
      PI.y <- 0
      xfunc <- fpdfx(virtualData[i,f1.mean],virtualData[i,f2.mean],virtualData[i,f1.sd],virtualData[i,f2.sd])
      yfunc <- fpdfy(virtualData[i,f1.mean],virtualData[i,f2.mean],virtualData[i,f1.sd],virtualData[i,f2.sd])
      try({
        repeat{
          j <- j+1
          PI.x <- PI.x+dblquad(xfunc,f1.limit[j],f1.limit[j+1],0,f2.limit[j])
          PI.y <- PI.y+dblquad(yfunc,f1.limit[j],f1.limit[j+1],0,f2.limit[j])
          if(j>length(f1.limit)-2) break()
        }
      })
      virtualData$PI.x[i] <- PI.x / virtualData$PI[i]
      virtualData$PI.y[i] <- PI.y / virtualData$PI[i]
      if(i>(dim(virtualData)[1]-1)) break()
      #if(i>100) break()
    }
  }else{
    pareto <- trainingData[paretoFront(trainingData[,f1.mean],trainingData[,f2.mean],min),]
    pareto <- pareto[order(pareto[,f1.mean]),]
    f1.limit <- c(0,pareto[,f1.mean],200)
    f2.limit <- c(pareto[,f2.mean],0)
    
    i <- 0
    repeat{
      i <- i+1
      j <- 0
      PI.x <- 0
      PI.y <- 0
      xfunc <- fpdfx(virtualData[i,f1.mean],virtualData[i,f2.mean],virtualData[i,f1.sd],virtualData[i,f2.sd])
      yfunc <- fpdfy(virtualData[i,f1.mean],virtualData[i,f2.mean],virtualData[i,f1.sd],virtualData[i,f2.sd])
      try({
        repeat{
          j <- j+1
          PI.x <- PI.x+dblquad(xfunc,f1.limit[j],f1.limit[j+1],f2.limit[j],200)
          PI.y <- PI.y+dblquad(yfunc,f1.limit[j],f1.limit[j+1],f2.limit[j],200)
          if(j>length(f1.limit)-2) break()
        }
      })
      virtualData$PI.x[i] <- PI.x / virtualData$PI[i]
      virtualData$PI.y[i] <- PI.y / virtualData$PI[i]
      if(i>(dim(virtualData)[1]-1)) break()
      #if(i>100) break()
    }
  }
  
  return(virtualData)
}

fpdf <- function(xmean,ymean,xsd,ysd){
  pdf <- function(x,y){
    return((1/(2*pi*xsd*ysd))*exp(-((x-xmean)^2/(2*xsd^2)+(y-ymean)^2/(2*ysd^2))))
  }
  return(pdf)
}

fpdfx <- function(xmean,ymean,xsd,ysd){
  pdf <- function(x,y){
    return(x*(1/(2*pi*xsd*ysd))*exp(-((x-xmean)^2/(2*xsd^2)+(y-ymean)^2/(2*ysd^2))))
  }
  return(pdf)
}

fpdfy <- function(xmean,ymean,xsd,ysd){
  pdf <- function(x,y){
    return(y*(1/(2*pi*xsd*ysd))*exp(-((x-xmean)^2/(2*xsd^2)+(y-ymean)^2/(2*ysd^2))))
  }
  return(pdf)
}

