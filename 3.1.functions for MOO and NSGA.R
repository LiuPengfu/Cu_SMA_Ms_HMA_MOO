
fn.hypervolume.easy <- function(data,reference){
  # data is for properties
  # for min-min bi-objective problem
  # rank by second property
  data <- data[order(data[,2],decreasing = T),]
  reference <- matrix(rep(reference,times=nrow(data)),ncol=2,byrow = T)
  distance <- abs(data - reference)
  n <- nrow(distance)
  if(n>1)
    # distance <- distance[order(distance[,2]),]
    
    i <- 1
  hv <- c()
  repeat{
    if(i==1)
      hv[i] <- distance[i,1]*distance[i,2]
    else if(i>1)
      hv[i] <- distance[i,1]*(distance[i,2]-distance[i-1,2])
    
    i <- i+1
    if(i>n)
      break
  }
  
  hypervol <- sum(hv)
  return(hypervol)
  
}


fn.paretoFront <- function(f1,f2,ID,method="min-min"){
  if(method == "min-min"){
    d = data.frame(f1,f2,ID)
    D = d[order(d$f1,d$f2,decreasing=FALSE),]
    front = D[which(!duplicated(cummin(D$f2))),]
  }
  else if(method == "max-max"){
    d = data.frame(f1,f2,ID)
    D = d[order(d$f1,d$f2,decreasing=TRUE),]
    front = D[which(!duplicated(cummax(D$f2))),]
  }
  else if(method=="min-max"){
    d = data.frame(f1,f2,ID)
    D = d[order(d$f1,d$f2,decreasing=FALSE),]
    front = D[which(!duplicated(cummax(D$f2))),]
  }
  else if(method=="max-min"){
    d = data.frame(f1,f2,ID)
    D = d[order(d$f1,d$f2,decreasing=TRUE),]
    front = D[which(!duplicated(cummin(D$f2))),]
  }
  front = front[order(front$ID,decreasing = F),]
  return(front$ID)
} 

fn.paretorank <- function(data,f1.name,f2.name,method="min-min"){
  
  i <- 1
  prank <- list()
  temp <- nrow(data)
  repeat{
    f1 <- data[,f1.name]
    f2 <- data[,f2.name]
    pareto <- fn.paretoFront(f1,f2,ID=data$ID,method)
    prank[[i]] <- pareto
    data <- data[-which(data$ID %in% pareto),]
    temp <- temp-length(pareto)
    print(paste("rank",i,"finish."))
    if(temp==0){
      break
    }
    i <- i+1
  }
  return(prank)
}

fai <- function(x){
  dnorm(x,mean=0,sd=1)
}

fai.cum <- function(x){
  pnorm(x,mean=0,sd=1) 
}

## mu for mean, sig for standard deviation
psi <- function(a,b,mu,sig){
  sig*fai((b-mu)/sig)+(a-mu)*fai.cum((b-mu)/sig)
}


fn.EHVI <- function(data,currentpareto,reference,intglim=-Inf){
  
  ## MOBGO-Yang min-min
  r <- reference # r for reference point c(X,Y)
  lim <- intglim # integral limit boundary set as -Inf or Inf in article
  #y <- currentpareto # n rows and 02 cols means n bi-objective points in pareto sets
  y <- rbind(c(r[1],lim),currentpareto,c(lim,r[2]))
  colnames(y) <- c("obj1","obj2")
  y1 <- y[,1]
  y2 <- y[,2]
  
  colnames(data) <- c("mu1","sd1","mu2","sd2") # data is for candidate with mean and sd
  mu1 <- as.vector(data$mu1)
  mu2 <- as.vector(data$mu2)
  sd1 <- as.vector(data$sd1)
  sd2 <- as.vector(data$sd2)
  n <- nrow(y) # actually n is number of pareto points + 2
  
  vn <- nrow(data) # number of virtual space
  EHVI <- vector(length = vn)
  
  j=1 # count for candidate points
  repeat{
    i=1 # number for pareto and limit points
    temp1 <- 0
    temp2 <- 0
    repeat{
      temp1 <- temp1 + (y1[i]-y1[i+1])*fai.cum((y1[i+1]-mu1[j])/sd1[j])*psi(y2[i+1],y2[i+1],mu2[j],sd2[j])
      temp2 <- temp2 + (psi(y1[i],y1[i],mu1[j],sd1[j])-psi(y1[i],y1[i+1],mu1[j],sd1[j]))*psi(y2[i+1],y2[i+1],mu2[j],sd2[j])
      i = i+1
      if(i > n-1){
        break
      }
      EHVI[j] <- temp1+temp2
    }
    
    j=j+1
    if(j>vn){
      break
    }
  }
  return(EHVI)
}



# fn.hypervolume.easy(data = property.pareto[,c("hysteresis","enthalpy")], 
#                     reference = c(max(rawdata$hysteresis),max(rawdata$enthalpy)))


fn.crowding <- function(data,pareto.num,f1.name,f2.name){
  
  pareto <- data[which(data$ID %in% pareto.num),]
  n <- nrow(pareto)
  # pareto <- pareto[,c("ID",f1.name,f2.name)]
  f1 <- pareto[,f1.name]
  f2 <- pareto[,f2.name]
  f1.scale <- paste(f1.name,"scale",sep = ".")
  f2.scale <- paste(f2.name,"scale",sep = ".")
  # cal.pareto <- pareto[,c("ID",f1.name,f2.name)]
  name <- c(f1.name,f2.name)
  samp <- sample(1:2,1)
  pareto <- pareto[order(pareto[,name[samp]],decreasing = T),]
  pareto[,c(f1.scale,f2.scale)] <- fn.maxmin.scale(pareto[,c(f1.name,f2.name)])
  temp1 <- pareto[1:(n-2),c(f1.scale,f2.scale)]
  temp2 <- pareto[2:(n-1),c(f1.scale,f2.scale)]
  temp3 <- pareto[3:n,c(f1.scale,f2.scale)]
  
  crowding <- c(Inf,rowSums(abs(temp2-temp1)+abs(temp3-temp2)),Inf)
  pareto$crowding <- crowding
  pareto <- pareto[order(pareto$crowding,decreasing = T),]
  pareto <- pareto[,-c((ncol(pareto)-2):ncol(pareto))]
  return(pareto)
}


fn.tourament <- function(data,prank,n.child,f1.name,f2.name){
  
  i=1
  parent <- data
  child <- NA
  count <- 0
  repeat{
    if((n.child - count) >= length(prank[[i]])){
      child <- rbind(child,data[which(data$ID %in% prank[[i]]),])
      count <- count + length(prank[[i]])
    }
    else{
      temp <- fn.crowding(data,pareto.num=prank[[i]],f1.name,f2.name)
      child <- rbind(child,temp[c(1:(n.child - count)),])
      break
    }
    i <- i + 1
  }
  child <- child[-1,]
  return(child)
}


fn.initial <- function(size,seed=10000){
  
  i <- 1 # random number
  j <- 1
  initial <- NA
  # data <- NA
  repeat{
    
    set.seed(seed+i)
    compositions <- c(Ni =runif(1,min = 25,max = 52),
                      Cu = runif(1,min = 0,max = 30),
                      Pd = runif(1,min = 0,max = 30),
                      Ti = runif(1,min = 25,max = 52),
                      Hf = runif(1,min = 0,max = 25),
                      Zr = runif(1,min = 0,max = 25))
    
    # compositions <- c(Ni =runif(1,min = 20,max = 52),
    #                   Co = runif(1,min = 0,max = 10),
    #                   Fe = runif(1,min = 0,max = 10),
    #                   Cu = runif(1,min = 0,max = 10),
    #                   Pd = runif(1,min = 0,max = 5),
    #                   Ti = runif(1,min = 20,max = 52),
    #                   Hf = runif(1,min = 0,max = 25),
    #                   Zr = runif(1,min = 0,max = 25))
    
    
    compositions <- round(compositions,1)*10
    
    # if(sum(compositions) == 100){
    #   compositions <- compositions # here no break
    # }else 
    if(sum(compositions) > 1000){
      repeat{
        if((sum(compositions) - 1000) >= (length(which(compositions!=0)))){
          # decide how many non-zero 
          compositions <- ifelse(compositions != 0, compositions-1,compositions)
          if(sum(compositions) == 1000)
            break
        }else if ((sum(compositions) - 1000) < (length(which(compositions!=0)))){
          comp.name <- names(compositions)
          compositions <- compositions[order(compositions,decreasing=T)]
          compositions[1:(sum(compositions)-1000)] <- compositions[1:(sum(compositions)-1000)]-1
          compositions <- compositions[comp.name]
          break
        }
      }
      
    }else if(sum(compositions) < 1000){
      repeat{
        if ((1000-sum(compositions)) >= (length(compositions))){
          compositions <- compositions+0.1
        } else if((1000-sum(compositions)) < (length(compositions))){
          comp.name <- names(compositions)
          compositions <- compositions[order(compositions,decreasing=T)]
          compositions[1:(1000-sum(compositions))] <- compositions[1:(1000-sum(compositions))]+1
          compositions <- compositions[comp.name]
          break
        }
      }
    }
    
    
    
    if(sum(compositions)==1000 & sum(compositions[c("Ti","Hf","Zr")])>=470 & sum(compositions[c("Ti","Hf","Zr")])<=520){
      initial <- rbind(initial,compositions)
      # initial <- ifelse(sum(duplicated(initial))==0,initial[,-nrow(initial)],initial)
      j <- j+1
      # print(c(i,j))
    }
    
    # if(j>1 & sum(duplicated(initial))==0){
    #   initial <- initial[,-nrow(initial)]
    # }else{
    #   initial <- initial
    #   j <- j+1
    # }
    
    
    if(j > size){
      break
    }else{
      i <- i+1
    }
  }
  initial <- initial[-1,]
  rownames(initial) <- NULL
  # return(list(data[-1,],initial))
  return(data.frame(initial))
}




fn.crossover <- function(parent,size){
  # SBXover
  # larger eta tend to generate children closer to parents
  # smaller eta tend to generate children far from parents
  child <- 0
  i <- 1
  parent <- as.matrix(parent)
  n <- nrow(parent)
  repeat{
    
    # set.seed(i)
    # i <- i+1
    # print(i)
    
    p <- sample(1:n,2,replace = F)
    
    p1 <- parent[p[1],]
    p2 <- parent[p[2],]
    
    difference <- length(which(abs(p1-p2)>1))
    
    ## p1 and p2 need to be different
    if(difference > 4){
      rand <- runif(1,0,1)
      eta <- 2
      if(rand <= 0.5){
        beta <- (2*rand)^(1/(1+eta))
      }else{
        beta <- (2*(1-rand))^(1/(1+eta))
      }
      
      c1 <- round(0.5*((1+beta)*p1+(1-beta)*p2))
      c2 <- round(0.5*((1-beta)*p1+(1+beta)*p2))
      
      
      if(sum(c1) > 1000){
        repeat{
          if((sum(c1) - 1000) >= length(which(c1!=0))){
            # decide how many non-zero
            c1 <- ifelse(c1 != 0, c1-1,c1)
            if(sum(c1) == 1000)
              break
          }else if ((sum(c1) - 1000) < length(which(c1!=0))){
            comp.name <- names(c1)
            c1 <- c1[order(c1,decreasing=T)]
            c1[1:(sum(c1)-1000)] <- c1[1:(sum(c1)-1000)]-1
            c1 <- c1[comp.name]
            break
          }
        }

      }else if(sum(c1) < 1000){
        repeat{
          if ((1000-sum(c1)) >= length(c1)){
            c1 <- c1+1
          } else if((1000-sum(c1)) < length(c1)){
            comp.name <- names(c1)
            c1 <- c1[order(c1,decreasing=T)]
            c1[1:(1000-sum(c1))] <- c1[1:(1000-sum(c1))]+1
            c1 <- c1[comp.name]
            break
          }
        }
      }

      if(sum(c2) > 1000){
        repeat{
          if((sum(c2) - 1000) >= length(which(c2!=0))){
            # decide how many non-zero
            c2 <- ifelse(c2 != 0, c2-1,c2)
            if(sum(c2) == 1000)
              break
          }else if ((sum(c2) - 1000) < length(which(c2!=0))){
            comp.name <- names(c2)
            c2 <- c2[order(c2,decreasing=T)]
            c2[1:(sum(c2)-1000)] <- c2[1:(sum(c2)-1000)]-1
            c2 <- c2[comp.name]
            break
          }
        }

      }else if(sum(c2) < 1000){
        repeat{
          if ((1000-sum(c2)) >= length(c2)){
            c2 <- c2+1
          } else if((1000-sum(c2)) < length(c2)){
            comp.name <- names(c2)
            c2 <- c2[order(c2,decreasing=T)]
            c2[1:(1000-sum(c2))] <- c2[1:(1000-sum(c2))]+1
            c2 <- c2[comp.name]
            break
          }
        }
      }
      
      
      
      if(sum(c1)==1000 & c1["Ni"]>=200 & c1["Ti"]>=200 & !duplicated(rbind(parent,c1))[n+1]){
        # child <- rbind(child,c1)
        parent <- rbind(parent,c1)
        parent <- parent[!duplicated(parent),]
        # print(nrow(parent)-n)
      }

      if(sum(c2)==1000 & c2["Ni"]>=200 & c2["Ti"]>=200 & !duplicated(rbind(parent,c2))[n+1]){
        # child <- rbind(child,c2)
        parent <- rbind(parent,c2)
        parent <- parent[!duplicated(parent),]
        # print(nrow(parent)-n)
        # print(i)
      }

      # nrow(child) < size
      
      # parent <- rbind(parent,c1,c2)
      # print(nrow(parent)-n)
      
      # child <- child[!duplicated(child),]
      # # print(nrow(child))
      # 
      # if(nrow(child)>size){
      #   break
      # }
      
      if((nrow(parent)-n) >= size){
        break
      }
    }
  }
  
  child <- parent[-c(1:n),]
  # child <- child[-1,]
  child <- child[1:size,]
  rownames(child) <- NULL
  # child <- child[c(1:size),]
  
  return(as.data.frame(child))
}


# initial <- fn.initial(size = 500)
# crossover <- fn.crossover(initial,size=500)
# crossover <- crossover[!duplicated(crossover),]
# sum(!duplicated(rbind(initial,crossover)))
# which(duplicated(rbind(initial,crossover)))
# length(!duplicated(rbind(initial,crossover)))

# a <- c(1,2,3,4,1,5)
# a[!duplicated(a)]


fn.mutation <- function(child,prob,eta=1){
  
  ## polynomial mutation
  
  # switch A+1 B-1
  eta <- 1
  i <- 1
  n <- nrow(child)
  child <- as.matrix(child)
  repeat{
    # print(i)
    decision <- runif(1)
    if(decision > prob){
      i <- i+1
    }else{
      
      u <- runif(1,0,1)
      if(u<0.5){
        delta <- (2*u)^(1/(eta))-1
      }else{
        delta <- 1-((2*(1-u))^(1/eta))
      }
      
      
      comp.name <- names(child[i,])
      # upper <- apply(child, 2, max)
      # lower <- apply(child, 2, min)
      range <- abs(apply(child, 2, max)-apply(child, 2, min))
      temp <- child[i,][order(child[i,],decreasing=T)]
      range <- range[order(child[i,],decreasing=T)]
      
      switch.pos <- sample(1:length(which(temp != 0)),2,replace = F)
      num <- sample(2,2,replace = F)
      
      mutation <- round(delta*range[switch.pos[num[1]]])
      
      if(mutation < 0){
        if(temp[switch.pos[num[1]]] + mutation < 0 & temp[switch.pos[num[2]]] - mutation > 0){
          temp[switch.pos[num[2]]] <- temp[switch.pos[num[2]]] + temp[switch.pos[num[1]]]
          temp[switch.pos[num[1]]] <- 0
        }else{
          temp[switch.pos[num[1]]] <- temp[switch.pos[num[1]]] + mutation
          temp[switch.pos[num[2]]] <- temp[switch.pos[num[2]]] - mutation
        }
      }else{
        if(temp[switch.pos[num[2]]] - mutation < 0){
          temp[switch.pos[num[2]]] <- temp[switch.pos[num[2]]] + temp[switch.pos[num[1]]]
          temp[switch.pos[num[1]]] <- 0
        }else{
          temp[switch.pos[num[1]]] <- temp[switch.pos[num[1]]] + mutation
          temp[switch.pos[num[2]]] <- temp[switch.pos[num[2]]] - mutation
        }
        
      }
      temp <- temp[comp.name]
      # if(temp[switch.pos[num[1]]] >= temp[switch.pos[num[2]]]){
      #   temp[switch.pos[num[1]]] <- temp[switch.pos[num[1]]] - u*temp[switch.pos[num[2]]]
      #   temp[switch.pos[num[2]]] <- temp[switch.pos[num[2]]] + m*temp[switch.pos[num[2]]]
      # }else{
      #   temp[switch.pos[num[1]]] <- temp[switch.pos[num[1]]] + m*temp[switch.pos[num[1]]]
      #   temp[switch.pos[num[2]]] <- temp[switch.pos[num[2]]] - m*temp[switch.pos[num[1]]]
      # }
      
      if(temp["Ni"]>=200 & temp["Ti"]>=200 & !duplicated(rbind(child,temp))[n+1]){
        child[i,] <- temp[comp.name]
        i <- i+1
      }
      else{
        i <- i
      }
    }
    
    if(i > nrow(child)){
      break
    }
    
  }
  return(as.data.frame(child))
}

# set.seed(10989)
# 
# initial <- fn.initial(size=500,seed=1)
# crossover <- fn.crossover(initial,size=500)
# mutation <- fn.mutation(crossover,prob=0.7)
# sum(duplicated(mutation==crossover))
# 
# sum(duplicated(rbind(crossover,mutation)))
# sum(mutation$Ni>=200)


fn.featured <- function(dataFeature,pop,features,elements,H){
  # pop <- as.matrix(pop)
  pop <- pop/10
  fea.name <- colnames(dataFeature)
  pop.fea <- fn.calFeature(dataReal=pop,features = features,elements = elements,H = H)
  
  max <- t(apply(dataFeature,2,max))
  max <- data.frame(max)
  max <- max[rep(seq_len(1), times = nrow(pop.fea)),]
  
  min <- t(apply(dataFeature,2,min))
  min <- data.frame(min)
  min <- min[rep(seq_len(1), times = nrow(pop.fea)),]
  
  pop.fea.scale <- (pop.fea[,fea.name]-min)/abs(max-min)
  
  return(pop.fea.scale)
}

fn.par.hys <- function(data,test,formula,times=500,boots.num=100,para=800,best=0){
  
  # boots.num <- list()
  # for (i in 1:1000) {
  #   set.seed(10000+i)
  #   boots.num[[i]] <- sample(nrow(test),nrow(test),replace = T)
  # }
  
  # fn.boots.hys <- function(data,test,formula,samp,para=c(2.5,0.1,2)){
  #   res.boots <- fn.gp(data[samp,-ncol(data)],data[samp,ncol(data)],test,para)
  #   return(res.boots)
  # }
  # 
  
  fn.boots.hys <- function(data,test,formula,samp,para=800){
    res.boots <- fn.rf(data[samp,],test,formula,para)
    return(res.boots)
  }
  
  # fn.gp = function(training.data.x, training.data.y,virtual.data.x,para=c(2,0.001,0.5)){ 
  #   model <- km(formula=~.,design=training.data.x, response=training.data.y,covtype = "exp",
  #               noise.var=rep(para[1],nrow(training.data.x)),lower = para[2], upper = para[3])
  #   
  #   prediction <- predict(model, virtual.data.x, "SK")
  #   # result <- data.frame(cbind(prediction$mean,prediction$sd))
  #   # colnames(result)=c("mean","sd")
  #   return(prediction$mean)
  # }
  
  # prediction <- NA
  fn.rf = function(data.train,data.test,formula,para=800){
    model = randomForest(formula, data=data.train,ntree=para)
    predResult = predict(model,data.test)
    return(data.frame(predResult))
  }
  # i <- 1
  # repeat {
  #   set.seed(100+i)
  #   samp <- boots.num[[i]]
  #   prediction <- cbind(prediction,fn.rf(data[samp,],test,formula,para=para))
  #   i <- i+1
  #   print(i)
  #   if(i>1000){
  #     break
  #   }
  # }
  # 
  library(foreach)
  
  cl<- makePSOCKcluster(detectCores()-8)
  registerDoParallel(cl)
  getDoParWorkers()
  time1 <- Sys.time()
  
  prediction <- foreach(
    num = c(1:times),
    .packages = "randomForest",
    .combine = "cbind",
    .errorhandling = "pass"
  )%dopar% fn.boots.hys(data,test,formula,samp = boots.num[[num]],para=para)
  
  time2 <- Sys.time()
  print(time2-time1)
  stopCluster(cl)
  gc()
  # prediction <- prediction[,-1]
  mean <- rowMeans(prediction)
  sd <- sqrt(rowSums((prediction-mean)^2)/1000)
  
  # property <- data[,ncol(data)]
  
  LCB <- mean - 1.96*sd
  
  
  # EI <- -sd*((min(property)-mean)/sd*pnorm((min(property)-mean)/sd) + dnorm((min(property)-mean)/sd))
  # EI <- -sd*((best-mean)/sd*pnorm((best-mean)/sd) + dnorm((best-mean)/sd))
  EI <- -sd*((min(mean)-mean-0.01)/sd*pnorm((min(mean)-mean-0.01)/sd) + dnorm((min(mean)-mean-0.01)/sd))
  
  return(cbind(hys.mean=mean,hys.sd=sd,hys.LCB=LCB,hys.EI=EI))
  
}


fn.par.hys(data.hys,data.hys,formula.f1,boots.num = 10)

fn.par.enth <- function(data,test,formula,times=500,boots.num,para=450,best=0){
  
  # boots.num <- list()
  # for (i in 1:1000) {
  #   set.seed(10000+i)
  #   boots.num[[i]] <- sample(nrow(test),nrow(test),replace = T)
  # }
  
  # fn.boots.enth <- function(data,test,formula,samp,para=450){
  #   res.boots <- fn.rf(data[samp,],test,formula,para)
  #   return(res.boots)
  # }
  
  
  
  fn.boots.enth <- function(data,test,formula,samp,para=450){
    res.boots <- fn.rf(data[samp,],test,formula,para)
    return(res.boots)
  }
  
  # prediction <- NA
  fn.rf = function(data.train,data.test,formula,para=450){
    model = randomForest(formula, data=data.train,ntree=para)
    predResult = predict(model,data.test)
    return(data.frame(predResult))
  }
  # i <- 1
  # repeat {
  #   set.seed(100+i)
  #   samp <- boots.num[[i]]
  #   prediction <- cbind(prediction,fn.rf(data[samp,],test,formula,para=para))
  #   i <- i+1
  #   print(i)
  #   if(i>1000){
  #     break
  #   }
  # }
  
  
  library(foreach)
  
  cl<- makePSOCKcluster(detectCores()-8)
  registerDoParallel(cl)
  getDoParWorkers()
  time1 <- Sys.time()
  
  prediction <- foreach(
    num = c(1:times),
    .packages = "randomForest",
    .combine = "cbind",
    .errorhandling = "pass"
  )%dopar% fn.boots.enth(data,test,formula,samp = boots.num[[num]],para)
  time2 <- Sys.time()
  print(time2-time1)
  stopCluster(cl)
  gc()
  
  # prediction <- prediction[,-1]
  mean <- rowMeans(prediction)
  sd <- sqrt(rowSums((prediction-mean)^2)/1000)

  # property <- data[,ncol(data)]
  
  LCB <- mean - 1.96*sd
  
  # EI <- -sd*((min(property)-mean)/sd*pnorm((min(property)-mean)/sd) + dnorm((min(property)-mean)/sd))
  EI <- -sd*((min(mean)-mean-0.01)/sd*pnorm((min(mean)-mean-0.01)/sd) + dnorm((min(mean)-mean-0.01)/sd))
  
  return(cbind(enth.mean=mean,enth.sd=sd,enth.LCB=LCB,enth.EI=EI))
  
}


fn.NSGA2 <- function(generation=300,i.seed=1000,b.num=1000,p.size=300,n.mp=150,m.prob=0.7,
                     data.hys,data.enth,fit.hys,fit.enth,
                     dataFeature,features,pop.elements,H,
                     gb.hys.name,gb.enth.name,para.hys,para.enth,best.hys,best.enth,
                     formula.f1,formula.f2){
  
  pareto.iter <- list()
  
  boots.num.hys <- list()
  for (j in 1:b.num) {
    set.seed(10000+j)
    boots.num.hys[[j]] <- sample(nrow(data.hys),nrow(data.hys),replace = T)
  }
  
  boots.num.enth <- list()
  for (j in 1:b.num) {
    set.seed(10000+j)
    boots.num.enth[[j]] <- sample(nrow(data.enth),nrow(data.enth),replace = T)
  }
  # i <- 1
  parent <- fn.initial(size=p.size,seed = i.seed)
  parent.fea <- fn.featured(dataFeature = dataFeature,pop=parent,features = features,elements = pop.elements,H=H)
  parent.fea.hys <- parent.fea[,gb.hys.name]
  parent.fea.enth <- parent.fea[,gb.enth.name]
  fitness.hys <- fn.par.hys(data.hys,parent.fea.hys,formula.f1,times=b.num,boots.num.hys,para = para.hys,best=best.hys)
  fitness.enth <- fn.par.enth(data.enth,parent.fea.enth,formula.f2,times=b.num,boots.num.enth,para = para.enth,best=best.enth)
  parent <- cbind(ID=1:nrow(parent),parent,fitness.hys,fitness.enth)
  # parent <- cbind(ID=1:nrow(parent),parent,hys.LCB=fitness.hys,enth.LCB=fitness.enth)
  
  parent.prk <- fn.paretorank(parent,f1.name=fit.hys,f2.name=fit.enth)
  
  for (i in 1:generation) {
    ## document each pareto
    pareto.iter[[i]] <- parent[parent.prk[[1]],-1]
    ## crossover and mutation
    matingPool <- fn.tourament(parent,parent.prk,n.child=n.mp,f1.name=fit.hys,f2.name=fit.enth)
    childX <- fn.crossover(parent=matingPool[,pop.elements],size=p.size)
    childM <- fn.mutation(child = childX,prob = m.prob)
    
    ## calculate the fitness
    childM.fea <- fn.featured(dataFeature = dataFeature,pop=childM,features = features,elements = pop.elements,H=H)
    childM.fea.hys <- childM.fea[,gb.hys.name]
    childM.fea.enth <- childM.fea[,gb.enth.name]
    ch.fitness.hys <- fn.par.hys(data.hys,childM.fea.hys,formula.f1,times=b.num,boots.num.hys,para = para.hys,best = best.hys)
    ch.fitness.enth <- fn.par.enth(data.enth,childM.fea.enth,formula.f2,times=b.num,boots.num.enth,para = para.enth,best=best.enth)
    childM <- cbind(ID=1:nrow(childM),childM,ch.fitness.hys,ch.fitness.enth)
    
    ## tournament selection
    parentNext <- rbind(parent[,-1],childM[,-1])
    rownames(parentNext) <- NULL
    parentNext <- parentNext[!duplicated(parentNext[,1:8]),]
    parentNext <- cbind(ID=1:nrow(parentNext),parentNext)
    newranking <- fn.paretorank(parentNext,f1.name=fit.hys,f2.name=fit.enth)
    
    ## new parent for next iteration
    parent <- fn.tourament(parentNext,newranking,n.child = p.size,f1.name=fit.hys,f2.name=fit.enth)
    rownames(parent) <- NULL
    
    parent <- cbind(ID=1:nrow(parent),parent[,-1])
    
    parent.prk <- fn.paretorank(parent,f1.name=fit.hys,f2.name=fit.enth)
    
    print(paste("The generation",i,"finished."))
  }
  
  return(pareto.iter)
  
}



fn.NSGA2.prt <- function(generation=300,i.seed=1000,b.num=1000,p.size=300,n.mp=150,m.prob=0.7,
                         pareto,data.hys,data.enth,fit.hys,fit.enth,
                         dataFeature,features,pop.elements,H,
                         gb.hys.name,gb.enth.name,para.hys,para.enth,best.hys,best.enth,
                         formula.f1,formula.f2){
  pareto.iter <- list()
  
  boots.num.hys <- list()
  for (j in 1:b.num) {
    set.seed(10000+j)
    boots.num.hys[[j]] <- sample(nrow(data.hys),nrow(data.hys),replace = T)
  }
  
  boots.num.enth <- list()
  for (j in 1:b.num) {
    set.seed(10000+j)
    boots.num.enth[[j]] <- sample(nrow(data.enth),nrow(data.enth),replace = T)
  }
  # i <- 1
  pareto <- pareto[,pop.elements]*10
  initial <- fn.initial(size=p.size-nrow(pareto),seed = i.seed)
  parent <- rbind(pareto,initial)
  parent.fea <- fn.featured(dataFeature = dataFeature,pop=parent,features = features,elements = pop.elements,H=H)
  parent.fea.hys <- parent.fea[,gb.hys.name]
  parent.fea.enth <- parent.fea[,gb.enth.name]
  fitness.hys <- fn.par.hys(data.hys,parent.fea.hys,formula.f1,times=b.num,boots.num.hys,para = para.hys,best=best.hys)
  fitness.enth <- fn.par.enth(data.enth,parent.fea.enth,formula.f2,times=b.num,boots.num.enth,para = para.enth,best=best.enth)
  parent <- cbind(ID=1:nrow(parent),parent,fitness.hys,fitness.enth)
  # parent <- cbind(ID=1:nrow(parent),parent,hys.LCB=fitness.hys,enth.LCB=fitness.enth)
  
  parent.prk <- fn.paretorank(parent,f1.name=fit.hys,f2.name=fit.enth)
  
  for (i in 1:generation) {
    ## document each pareto
    pareto.iter[[i]] <- parent[parent.prk[[1]],-1]
    ## crossover and mutation
    matingPool <- fn.tourament(parent,parent.prk,n.child=n.mp,f1.name=fit.hys,f2.name=fit.enth)
    childX <- fn.crossover(parent=matingPool[,pop.elements],size=p.size)
    childM <- fn.mutation(child = childX,prob = m.prob)
    
    ## calculate the fitness
    childM.fea <- fn.featured(dataFeature = dataFeature,pop=childM,features = features,elements = pop.elements,H=H)
    childM.fea.hys <- childM.fea[,gb.hys.name]
    childM.fea.enth <- childM.fea[,gb.enth.name]
    ch.fitness.hys <- fn.par.hys(data.hys,childM.fea.hys,formula.f1,times=b.num,boots.num.hys,para = para.hys,best = best.hys)
    ch.fitness.enth <- fn.par.enth(data.enth,childM.fea.enth,formula.f2,times=b.num,boots.num.enth,para = para.enth,best=best.enth)
    childM <- cbind(ID=1:nrow(childM),childM,ch.fitness.hys,ch.fitness.enth)
    
    ## tournament selection
    parentNext <- rbind(parent[,-1],childM[,-1])
    rownames(parentNext) <- NULL
    parentNext <- parentNext[!duplicated(parentNext[,1:8]),]
    parentNext <- cbind(ID=1:nrow(parentNext),parentNext)
    newranking <- fn.paretorank(parentNext,f1.name=fit.hys,f2.name=fit.enth)
    
    ## new parent for next iteration
    parent <- fn.tourament(parentNext,newranking,n.child = p.size,f1.name=fit.hys,f2.name=fit.enth)
    rownames(parent) <- NULL
    
    parent <- cbind(ID=1:nrow(parent),parent[,-1])
    
    parent.prk <- fn.paretorank(parent,f1.name=fit.hys,f2.name=fit.enth)
    
    print(paste("The generation",i,"finished."))
  }
  
  return(pareto.iter)
  
  
  
  
}




fn.uncertainty <- function(sd1,sd2){
  
  uncertainty <- 4*sd1*sd2
  return(uncertainty)
  
}

# fn.distance <- function(f1,f2,target){
#   f1 <- c(f1,target[1])
#   f2 <- c(f2,target[2])
#   distance <- sqrt(((f1[-length(f1)]-min(f1))/(max(f1)-min(f1)))^2+((f2[-length(f1)]-min(f2))/(max(f2)-min(f2)))^2)
#   return(distance)
# }


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