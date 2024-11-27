set.seed(12345)

### hysteresis model
## support vector machine rbf
paralist.svr <- data.frame(expand.grid(gamma=c(seq(10,200,10)),
                                       cost=c(seq(10,200,10))))

## use 8 logical processor
cl<- makePSOCKcluster(detectCores()-8) 
registerDoParallel(cl)
getDoParWorkers()
time1 <- Sys.time()

para.Ms.svr <- foreach(
  num = c(1:nrow(paralist.svr)),
  .packages = "e1071",
  .combine = "rbind"
)%dopar% fn.para.svr.decision(data.Ms,formula.f1,k=10,boots=100,paralist = paralist.svr[num,])

time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc()

# error.Ms <- para.Ms.svr$cv.mae+para.Ms.svr$boots.mae
best.Ms.svr <- para.Ms.svr[which(para.Ms.svr$rmse==min(para.Ms.svr$rmse)),]
# best.Ms.svr.boots <- para.Ms.svr[which(para.Ms.svr$rmse==min(para.Ms.svr$rmse)),]

# write.csv(best.Ms.svr,"best.Ms.svr.csv")
# best.Ms.svr <- read.csv("best.Ms.svr.csv")[,-1]
# fn.para.svr.decision(data.Ms,formula.f1,k=10,paralist = paralist.svr[18,])


## random forest
paralist.rf <- data.frame(ntrees=seq(50,1000,by=50))
cl<- makePSOCKcluster(detectCores()-8) 
registerDoParallel(cl)
getDoParWorkers()
time1 <- Sys.time()

para.Ms.rf <- foreach(
  num = c(1:nrow(paralist.rf)),
  .packages = "randomForest",
  .combine = "rbind"
)%dopar% fn.para.rf.decision(data.Ms,formula.f1,k=10,boots=100,paralist = paralist.rf[num,])

time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc()

best.Ms.rf <- para.Ms.rf[which(para.Ms.rf$rmse==min(para.Ms.rf$rmse)),]
# write.csv(best.Ms.rf,"best.Ms.rf.csv")
# best.Ms.rf <- read.csv("best.Ms.rf.csv")[,-1]



## xgboost

paralist.xgb <- data.frame(expand.grid(depth=c(3:7),nround=seq(20,50,by=5)))

cl<- makePSOCKcluster(detectCores()-8)  
registerDoParallel(cl)
getDoParWorkers()
time1 <- Sys.time()

para.Ms.xgb <- foreach(
  num = c(1:nrow(paralist.xgb)),
  .packages = "xgboost",
  .combine = "rbind",
  .errorhandling	= "remove"
)%dopar% fn.para.xgb.decision(data.Ms,formula.f1,k=10,boots=100,paralist = paralist.xgb[num,])

time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc()

para.Ms.xgb <- data.frame(para.Ms.xgb)
best.Ms.xgb <- para.Ms.xgb[which(para.Ms.xgb$rmse==min(para.Ms.xgb$rmse)),]
best.Ms.xgb <- as.matrix(best.Ms.xgb)


### HMA model
## support vector machine rbf
paralist.svr <- data.frame(expand.grid(gamma=c(seq(1,20,1)),
                                       cost=c(seq(1,20,1))))
cl<- makePSOCKcluster(detectCores()-8)  
registerDoParallel(cl)
getDoParWorkers()
time1 <- Sys.time()

para.HMA.svr <- foreach(
  num = c(1:nrow(paralist.svr)),
  .packages = "e1071",
  .combine = "rbind"
)%dopar% fn.para.svr.decision(data.HMA,formula.f2,k=10,boots=100,paralist = paralist.svr[num,])

time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc()

best.HMA.svr <- para.HMA.svr[which(para.HMA.svr$rmse==min(para.HMA.svr$rmse)),]
# best.HMA.svr.boots <- para.HMA.svr[which(para.HMA.svr$boots.rmse==min(para.HMA.svr$boots.rmse)),]
# write.csv(best.HMA.svr,"best.HMA.svr.csv")
# best.HMA.svr <- read.csv("best.HMA.svr.csv")[,-1]


## random forest
paralist.rf <- data.frame(ntrees=seq(50,1000,by=50))
cl<- makePSOCKcluster(detectCores()-8) 
registerDoParallel(cl)
getDoParWorkers()
time1 <- Sys.time()

para.HMA.rf <- foreach(
  num = c(1:nrow(paralist.rf)),
  .packages = "randomForest",
  .combine = "rbind"
)%dopar% fn.para.rf.decision(data.HMA,formula.f2,k=10,boots=100,paralist = paralist.rf[num,])

time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc()

best.HMA.rf <- para.HMA.rf[which(para.HMA.rf$rmse==min(para.HMA.rf$rmse)),]
# best.HMA.rf.boots <- para.HMA.rf[which(para.HMA.rf$boots.rmse==min(para.HMA.rf$boots.rmse)),]
# write.csv(best.HMA.rf,"best.HMA.rf.csv")
# best.HMA.rf <- read.csv("best.HMA.rf.csv")[,-1]


paralist.xgb <- data.frame(expand.grid(depth=c(3:7),nround=seq(20,50,by=5)))

cl<- makePSOCKcluster(detectCores()-8)  
registerDoParallel(cl)
getDoParWorkers()
time1 <- Sys.time()

para.HMA.xgb <- foreach(
  num = c(1:nrow(paralist.xgb)),
  .packages = "xgboost",
  .combine = "rbind",
  .errorhandling	= "remove"
)%dopar% fn.para.xgb.decision(data.HMA,formula.f2,k=10,boots=100,paralist = paralist.xgb[num,])

time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc()

para.HMA.xgb <- data.frame(para.HMA.xgb)
best.HMA.xgb <- para.HMA.xgb[which(para.HMA.xgb$rmse==min(para.HMA.xgb$rmse)),]
best.HMA.xgb <- as.matrix(best.HMA.xgb)