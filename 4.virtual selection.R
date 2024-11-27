vir.elements <- c("Ni","Co","Fe","Cu","Pd","Ti","Hf","Zr")
vir <- fn.virtualdata()





vir <- fn.hys.feature(dataReal = vir,features = features,
                      elements = vir.elements,H = H)
vir <- fn.enth.feature(dataReal = vir,features = features,
                       elements = vir.elements,H = H)

virFeature <- vir[,-c(1:length(vir.elements))]
vir.fea.name <- colnames(virFeature)

### scalarization max-min
vir.scale <- fn.maxmin.scale(virFeature)
rownames(vir.scale) <- NULL
vir.Featurescale <- vir.scale[,vir.fea.name]
vir.hys <- vir.Featurescale[,gb.hys.name]
vir.enth <- vir.Featurescale[,gb.enth.name]
# 

set.seed(111111)
sp.num <- fn.vec.split(k=100,datasize = nrow(vir.hys))
# sp.enth.num <- fn.vec.split(k=100,datasize = nrow(vir.enth))

library("doParallel")      #????doParallel??????֮??ע??????
library("foreach")         #????foreach??
cl<- makeCluster(detectCores()-7)      
registerDoParallel(cl)       #???н???ע??
getDoParWorkers()  #  #?鿴ע???˶??ٸ???
time1 <- Sys.time()

vir.boots.hys <- foreach(num=sp.num, 
                         .combine = "rbind",
                         .packages = c("randomForest")
)%dopar% fn.boots.vir(data=data.hys,test = vir.hys[num,],
                      formula = formula.f1,boots = 1000,method = "rf",rf=1000)#foreach?кܶ????????Ե???


time2 <- Sys.time()
time2-time1

stopCluster(cl)
gc()

# enth.vir.boots <- fn.boots.vir(data = data.enth,test = virtual.enth[1:20,],formula = formula.f2,
#                                boots = 10,method = "gp")
# # save.image("5.7.RData")
# gc(reset = T)



library("doParallel")      #????doParallel??????֮??ע??????
library("foreach")         #????foreach??
cl<- makeCluster(detectCores()-7)      
registerDoParallel(cl)       #???н???ע??
getDoParWorkers()  #  #?鿴ע???˶??ٸ???
time1 <- Sys.time()

vir.boots.enth <- foreach(num = sp.num, 
                          .combine = "rbind",
                          .packages = c("randomForest")
)%dopar% fn.boots.vir(data=data.enth,test = vir.enth[num,],
                      formula = formula.f2,boots = 1000,method = "rf",rf=950)#foreach?кܶ????????Ե???


time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc(reset = T)

save.image("230716.RData")

# 
# time1 <- Sys.time()
vir.boots.hys <- fn.boots.vir(data=data.hys,test = vir.hys,
                              formula = formula.f1,boots = 1000,method = "rf",rf=1000)
# 
# time2 <- Sys.time()
# time2-time1
# 
# 
# time1 <- Sys.time()
# 
vir.boots.enth <- fn.boots.vir(data=data.enth,test = vir.enth,
                               formula = formula.f2,boots = 1000,method = "rf",rf=950)
# 
# time2 <- Sys.time()
# time2-time1
best <- c(15,-21.09)

vir <- cbind(ID=c(1:nrow(vir)),vir[,vir.elements])
vir$hys.mean <- vir.boots.hys$mean
vir$hys.sd <- vir.boots.hys$sd
vir$hys.LCB <- vir$hys.mean - 1.96*vir$hys.sd

# hys.EI <- vir$hys.sd*((best[1]-vir$hys.mean)/vir$hys.sd*pnorm((best[1]-vir$hys.mean)/vir$hys.sd)+
#         dnorm((best[1]-vir$hys.mean)/vir$hys.sd))
# 
# str(a)

vir$hys.EI <- vir$hys.sd*((best[1]-vir$hys.mean)/vir$hys.sd*pnorm((best[1]-vir$hys.mean)/vir$hys.sd)+
                            dnorm((best[1]-vir$hys.mean)/vir$hys.sd))


vir$enth.mean <- vir.boots.enth$mean
vir$enth.sd <- vir.boots.enth$sd
vir$enth.LCB <- vir$enth.mean - 1.96*vir$enth.sd
vir$enth.EI <- vir$enth.sd*((best[2]-vir$enth.mean)/vir$enth.sd*pnorm((best[2]-vir$enth.mean)/vir$enth.sd)+
                              dnorm((best[2]-vir$enth.mean)/vir$enth.sd))
vir$uncertainty <- 4*vir$hys.sd*vir$enth.sd


vir.pareto.num <- fn.paretoFront(f1=vir$hys.mean,f2=vir$enth.mean,ID=vir$ID)
vir.pareto <- vir[vir.pareto.num,]

plot(vir.pareto$hys.mean,vir.pareto$enth.mean,xlim=c(0,60),ylim=c(-40,0))
# points(pareto.m.all$hys.mean,pareto.m.all$enth.mean,xlim=c(0,60),ylim=c(-40,0))
points(raw.pareto$hysteresis,raw.pareto$enthalpy,col="red")




rawdata.LCB <- data.frame(ID=c(1:nrow(rawdata)),
                          hys.LCB=data.boots.hys$mean-1.96*data.boots.hys$sd,
                          enth.LCB=data.boots.enth$mean-1.96*data.boots.enth$sd)
LCB.pareto.num <- fn.paretoFront(f1=rawdata.LCB$hys.LCB,f2=rawdata.LCB$enth.LCB,ID=rawdata.LCB$ID)
LCB.pareto <- rawdata.LCB[LCB.pareto.num,]
LCB.pareto <- LCB.pareto[order(LCB.pareto$hys.LCB,decreasing = T),]
hypervolume.LCB <- fn.hypervolume.easy(data=LCB.pareto[,c("hys.LCB","enth.LCB")],reference = c(50,0))


rawdata.mean <- data.frame(ID=c(1:nrow(rawdata)),
                           hys.mean=data.boots.hys$mean,
                           enth.mean=data.boots.enth$mean)

mean.pareto.num <- fn.paretoFront(f1=rawdata.mean$hys.mean,f2=rawdata.mean$enth.mean,ID=rawdata.mean$ID)
mean.pareto <- rawdata.mean[mean.pareto.num,]
mean.pareto <- mean.pareto[order(mean.pareto$hys.mean,decreasing = T),]
hypervolume.mean <- fn.hypervolume.easy(data=mean.pareto[,c("hys.mean","enth.mean")],reference = c(50,0))

property.pareto <- property.pareto[order(property.pareto$hysteresis,decreasing = T),]
hypervolume.prop <- fn.hypervolume.easy(data=property.pareto[,c("hysteresis","enthalpy")],reference = c(50,0))