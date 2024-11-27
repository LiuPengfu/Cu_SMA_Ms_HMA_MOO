### feature calculation
dataReal <- cbind(data.at,rawdata[,-c(1:length(data.elements))])

dataReal <- fn.calFeature(dataReal = dataReal,features = features,
                          elements = data.elements,H = H)

dataFeature <- dataReal[,-c(1:(length(data.elements)+length(data.prop)+length(data.proc)))]
# dataFeature <- dataReal[,-c(1:(length(data.elements)+length(data.prop)))]
feature.name <- colnames(dataFeature)

### scalarization max-min
data.scale <- fn.maxmin.scale(dataFeature)
rownames(data.scale) <- NULL
# write.csv(data.all.scale,"data.all.scale.csv")


data.Featurescale <- data.scale[,feature.name]

data.Featurescale$Ms <- rawdata$Ms
data.Featurescale$HMA <- rawdata$HMA
fea.abbr <- NULL
for (i in c(1:length(colnames(dataFeature)))) {
  temp <- paste("F",i,sep="")
  fea.abbr <- c(fea.abbr,temp)
}


M <- cor(data.Featurescale[,feature.name])


write.csv(data.frame(features=colnames(dataFeature),ID=fea.abbr),"feature name.csv")

# data.Featurescale <- rawdata[,c(data.elements,data.proc)]


### Pearson

feature.name.pearson<-c('S.mix','r.atom.mix','delta.r.mix','lamda.mix','Tm.mix','delta.x.mix','H.mix',
                        'ou.mix','delta.ym.mix','delta.Poisson.Ratio.mix','cs.mix','delta.cs.mix',
                        'delta.rm.mix','bm.mix','delta.bm.mix','Efus.mix','delta.Efus.mix','delta.s.solid.mix',
                        'delta.Debye.mix','wf.mix','delta.wf.mix','amass.mix','delta.conT.mix','cm.mix','delta.cm.mix',
                        'delta.m.hcap.mix','hom.mix','delta.hom.mix','delta.mcs.mix','vp.mix','delta.vp.mix',
                        'vatom.mix','delta.vatom.mix')


data.Featurescale <- data.Featurescale[,feature.name.pearson]
data.Featurescale$Ms <- rawdata$Ms

data.Featurescale$HMA <- rawdata$HMA


M <- cor(data.Featurescale[,feature.name])

colnames(M) <- fea.abbr
rownames(M) <- fea.abbr
corrplot(M,method="color",type="full",tl.col = "black",order = "hclust",addrect = 4, tl.cex = 0.7,tl.srt = 60)

write.csv(M,"M.csv")

library(ggcorrplot)
library(ggthemes)
ggcorrplot(M,method = "square",ggtheme = "theme_bw",hc.order=TRUE,hc.method = "ward.D",type="full")



### dimension reduction of features
## hysteresis
# relative influence
set.seed(12345)
gb.Ms=gbm(Ms~.,distribution = "gaussian",
           data=data.Featurescale[!is.na(data.Featurescale$Ms),c(1:(ncol(data.Featurescale)-2),ncol(data.Featurescale)-1)],
           n.trees = 1000,interaction.depth = 7,shrinkage = 0.01,cv.folds = 10)
summary(gb.Ms)

rel.inf.Ms <- summary(gb.Ms)
write.csv(rel.inf.Ms,"rel.inf.Ms.csv")
barplot(rel.inf.Ms$rel.inf, names.arg = rel.inf.Ms$var, horiz = T,col = "blue", ylim = c(0,35), main = "Relative importance")
box(lwd=3)
# gb.Ms.name <- rel.inf.Ms$var[which(rel.inf.Ms$rel.inf > 4)]
gb.Ms.name <- rel.inf.Ms$var[1:6]
# write.csv(gb.Ms.name,"gb.Ms.name.csv")
# gb.Ms.name87 <- gb.Ms.name
# write.csv(gb.Ms.name,"gb.Ms.name.csv")


## HMA
# relative influence
set.seed(12345)
gb.HMA=gbm(HMA~.,distribution = "gaussian",
            data=data.Featurescale[!is.na(data.Featurescale$HMA),c(1:(ncol(data.Featurescale)-2),ncol(data.Featurescale))],
            n.trees = 1000,interaction.depth = 4,shrinkage = 0.01,cv.folds = 10,n.minobsinnode=2)
summary(gb.HMA)
# gb.HMA$cv.error

rel.inf.HMA <- summary(gb.HMA)
write.csv(rel.inf.HMA,"rel.inf.HMA.csv")
barplot(rel.inf.HMA$rel.inf, names.arg = rel.inf.HMA$var, col = "darkgreen", ylim = c(0,20), main = "Relative importance")
box(lwd=3)
# gb.HMA.name <- rel.inf.HMA$var[which(rel.inf.HMA$rel.inf > 5)]
gb.HMA.name <- rel.inf.HMA$var[1:6]
# "hom.mix"       "delta.cp.mix"  "m.hcap.mix"    "delta.hmo.mix" "slope" 
# write.csv(gb.HMA.name,"gb.HMA.name.csv")


# ## rfe
library(caret)
control = rfeControl(functions=rfFuncs, method="cv", number=10)
# control = rfeControl(functions=rfFuncs, method="LOOCV")
# fea.rfe = rfe(data.train.pea[,1:7], data.train.pea[,8], sizes=c(1:7), rfeControl=control)
rfe.Ms = rfe(data.Featurescale[!is.na(data.Featurescale$Ms),c(1:(ncol(data.Featurescale)-2))],
             data.Featurescale$Ms[!is.na(data.Featurescale$Ms)],
             sizes=c(1:33),rfeControl=control)
print(rfe.Ms)
predictors(rfe.Ms)
plot(rfe.Ms, type=c("g", "o"))
#
rfe.HMA = rfe(data.Featurescale[!is.na(data.Featurescale$HMA),c(1:(ncol(data.Featurescale)-2))],
              data.Featurescale$HMA[!is.na(data.Featurescale$HMA)],
              sizes=c(1:33),
               rfeControl=control)
print(rfe.HMA)
predictors(rfe.HMA)
plot(rfe.HMA, type=c("g", "o"))



rfRFE <-  list(summary = defaultSummary,
               fit = function(x, y, first, last, ...){
                 library(xgboost)
                 randomForest(x, y, importance = first, ...)
               },
               pred = function(object, x)  predict(object, x),
               rank = function(object, x, y) {
                 vimp <- varImp(object)
                 vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
                 vimp$var <- rownames(vimp)                  
                 vimp
               },
               selectSize = pickSizeBest,
               selectVar = pickVars)


control = rfeControl(functions=rfFuncs, method="cv", number=10)
# fea.rfe = rfe(data.train.pea[,1:7], data.train.pea[,8], sizes=c(1:7), rfeControl=control)
rfe.Ms = rfe(data.Featurescale[!is.na(data.Featurescale$Ms),rel.inf.Ms$var[1:30]],
             data.Featurescale$Ms[!is.na(data.Featurescale$Ms)],metric = "RMSE",
             sizes=c(1:30),rfeControl=control)
print(rfe.Ms)
predictors(rfe.Ms)
plot(rfe.Ms, type=c("g", "o"))
#
rfe.HMA = rfe(data.Featurescale[!is.na(data.Featurescale$HMA),rel.inf.HMA$var[1:30]],
              data.Featurescale$HMA[!is.na(data.Featurescale$HMA)],
              sizes=c(1:30),metric = "RMSE",
              rfeControl=control)
print(rfe.HMA)
predictors(rfe.HMA)
plot(rfe.HMA, type=c("g", "o"))



unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()



# maxmin scale selection
formula.f1 <- Ms~.
formula.f2 <- HMA~.

data.Ms <- data.Featurescale[!is.na(data.Featurescale$Ms),c(gb.Ms.name,"Ms")]
rownames(data.Ms) <- NULL
data.HMA <- data.Featurescale[!is.na(data.Featurescale$HMA),c(gb.HMA.name,"HMA")]
rownames(data.HMA) <- NULL



### Model Comparison for Hysteresis
## CV for hysteresis

set.seed(123)
k <- 10

# Ms
data.Ms <- data.Ms[-c(15,16,39,70),]
set.seed(1234)
cv.Ms.svr <- fn.cv(data.Ms,formula.f1,k=10,"svr.rbf",svr = c(1,1.6))
cv.Ms.rf <- fn.cv(data.Ms,formula.f1,k=10,"rf",rf=c(4,600))
cv.Ms.gp <- fn.cv(data.Ms,formula.f1,k=10,"gp",gp=c(5e-3,2,3e-3))
cv.Ms.xgb <- fn.cv(data.Ms,formula.f1,k=10,"xgb",xgb = c(5,50))
cv.Ms.lm <- fn.cv(data.Ms,formula.f1,k=10,"lm")
cv.Ms.nnet <- fn.cv(data.Ms,formula.f1,k=10,"nnet")
cv.Ms.svrl <- fn.cv(data.Ms,formula.f1,k=10,"svr.lin",svrl = c(0.01,0.03))
cv.Ms.svrp <- fn.cv(data.Ms,formula.f1,k=10,"svr.poly",svrp = c(0.1,0.9))

c(cv.Ms.svr$rmse,cv.Ms.rf$rmse,cv.Ms.gp$rmse,cv.Ms.xgb$rmse)

c(cv.Ms.svr$rmse,cv.Ms.rf$rmse,cv.Ms.gp$rmse,cv.Ms.xgb$rmse,
  cv.Ms.lm$rmse,cv.Ms.nnet$rmse,cv.Ms.svrl$rmse,cv.Ms.svrp$rmse)

c(cv.Ms.svr$mse,cv.Ms.rf$mse,cv.Ms.gp$mse,cv.Ms.xgb$mse,
  cv.Ms.lm$mse,cv.Ms.nnet$mse,cv.Ms.svrl$mse,cv.Ms.svrp$mse)
mean(cv.Ms.svr$rmse.all)
sd(cv.Ms.svr$mse.all)
sd(cv.Ms.svr$rmse.all)


samp <- sample(nrow(data.Ms),0.8*nrow(data.Ms),replace = F)
Ms82.svr <- fn.svr.rbf(data.Ms[samp,],data.Ms,formula.f1,c(1,1.6))
plot(unlist(data.Ms[samp,"Ms"]),unlist(Ms82.svr[samp,]),xlim=c(200,600),ylim=c(200,600))
points(unlist(data.Ms[-samp,"Ms"]),unlist(Ms82.svr[-samp,]),col="blue")
abline(0,1,col="red",lwd=2)
fn.R2.single(Ms82.svr$predResult,data.Ms$Ms)
fn.R2.single(HMA82.svr$predResult,data.HMA$HMA)

write.csv(cbind(mea=data.Ms[samp,"Ms"],pred=Ms82.svr[samp,]),"Ms82.svr-80.csv")
write.csv(cbind(mea=data.Ms[-samp,"Ms"],pred=Ms82.svr[-samp,]),"Ms82.svr-20.csv")


write.csv(cbind(c("svr","rf","gp","xgb"),c(cv.Ms.svr$mse,cv.Ms.rf$mse,cv.Ms.gp$mse,cv.Ms.xgb$mse)),"Ms.mse.csv")
write.csv(cbind(c("svr","rf","gp","xgb"),c(cv.Ms.svr$rmse,cv.Ms.rf$rmse,cv.Ms.gp$rmse,cv.Ms.xgb$rmse)),"Ms.rmse.csv")
write.csv(cbind(c("svr","rf","gp","xgb"),
                c(sd(cv.Ms.svr$rmse.all),sd(cv.Ms.rf$rmse.all),
                  sd(cv.Ms.gp$rmse.all),sd(cv.Ms.xgb$rmse.all))),"RMSE.sd.Ms.csv")


fn.R2.single(cv.Ms.svr$res$prediction,data.Ms$Ms)
fn.R2.single(cv.Ms.rf$res$prediction,data.Ms$Ms)

write.csv(cbind(c("svr","rf","gp","xgb"),
                c(fn.R2.single(cv.Ms.svr$res$prediction,data.Ms$Ms),
                  fn.R2.single(cv.Ms.rf$res$prediction,data.Ms$Ms),
                  fn.R2.single(cv.Ms.gp$res$prediction,data.Ms$Ms),
                  fn.R2.single(cv.Ms.xgb$res$prediction,data.Ms$Ms))),"Ms.R2.csv")
write.csv(cbind(cv.Ms.svr$res$prediction,data.Ms$Ms),"Ms.CV.csv")

## HMA
data.HMA <- data.HMA[-c(1,5,24,25),]

cv.HMA.svr <- fn.cv(data.HMA,formula.f2,k=10,"svr.rbf",svr = c(1,5))
cv.HMA.rf <- fn.cv(data.HMA,formula.f2,k=10,"rf",rf=c(5,700))
cv.HMA.gp <- fn.cv(data.HMA,formula.f2,k=10,"gp",gp=c(1e-5,2,1e-3))
cv.HMA.xgb <- fn.cv(data.HMA,formula.f2,k=10,"xgb",xgb = c(4,30))
cv.HMA.lm <- fn.cv(data.HMA,formula.f2,k=10,"lm")
cv.HMA.nnet <- fn.cv(data.HMA,formula.f2,k=10,"nnet")
cv.HMA.svrl <- fn.cv(data.HMA,formula.f2,k=10,"svr.lin",svrl = c(1,2))
cv.HMA.svrp <- fn.cv(data.HMA,formula.f2,k=10,"svr.poly",svrp = c(0.1,1.5))
# colnames(data.HMA)


c(cv.HMA.svr$rmse,cv.HMA.rf$rmse,cv.HMA.gp$rmse,cv.HMA.xgb$rmse,
  cv.HMA.lm$rmse,cv.HMA.nnet$rmse,cv.HMA.svrl$rmse,cv.HMA.svrp$rmse)

c(cv.HMA.svr$mse,cv.HMA.rf$mse,cv.HMA.gp$mse,cv.HMA.xgb$mse,
  cv.HMA.lm$mse,cv.HMA.nnet$mse,cv.HMA.svrl$mse,cv.HMA.svrp$mse)

write.csv(cbind(c("svr","rf","gp","xgb"),c(cv.HMA.svr$mse,cv.HMA.rf$mse,cv.HMA.gp$mse,cv.HMA.xgb$mse)),"HMA.mse.csv")
write.csv(cbind(c("svr","rf","gp","xgb"),c(cv.HMA.svr$rmse,cv.HMA.rf$rmse,cv.HMA.gp$rmse,cv.HMA.xgb$rmse)),"HMA.rmse.csv")
write.csv(cbind(c("svr","rf","gp","xgb"),
                c(fn.R2.single(cv.HMA.svr$res$prediction,data.HMA$HMA),
                  fn.R2.single(cv.HMA.rf$res$prediction,data.HMA$HMA),
                  fn.R2.single(cv.HMA.gp$res$prediction,data.HMA$HMA),
                  fn.R2.single(cv.HMA.xgb$res$prediction,data.HMA$HMA))),"HMA.R2.csv")
write.csv(cbind(cv.HMA.svr$res$prediction,data.HMA$HMA),"HMA.CV.csv")


write.csv(cbind(c("svr","rf","gp","xgb"),
                c(sd(cv.HMA.svr$rmse.all),sd(cv.HMA.rf$rmse.all),
                  sd(cv.HMA.gp$rmse.all),sd(cv.HMA.xgb$rmse.all))),"RMSE.sd.HMA.csv")


samp <- sample(nrow(data.HMA),0.8*nrow(data.HMA),replace = F)
HMA82.svr <- fn.svr.rbf(data.HMA[samp,],data.HMA,formula.f2,c(1,5))
plot(unlist(data.HMA[samp,"HMA"]),unlist(HMA82.svr[samp,]),xlim=c(2,12),ylim=c(2,12))
points(unlist(data.HMA[-samp,"HMA"]),unlist(HMA82.svr[-samp,]),col="blue")
abline(0,1,col="red",lwd=2)
write.csv(cbind(mea=data.HMA[samp,"HMA"],pred=HMA82.svr[samp,]),"HMA82.svr-80.csv")
write.csv(cbind(mea=data.HMA[-samp,"HMA"],pred=HMA82.svr[-samp,]),"HMA82.svr-20.csv")

# Rsqd <- expression(bold(R)^bold("2"))
# # Rsqd <- expression(R^2)
# print(Rsqd)

