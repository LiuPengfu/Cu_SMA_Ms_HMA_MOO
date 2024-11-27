## virtual space
# vir <- expand.grid(Al=seq(10,25,by=0.1),Ni=seq(0,4,by=0.1),Mn=seq(0,15,by=0.1),Zr=c(0,2,by=0.1))
vir <- expand.grid(Al=seq(10,25,by=0.1),Ni=seq(0,4,by=0.1),Mn=seq(0,15,by=0.1),V=c(0,10,by=0.1))
vir$Cu <- 100-rowSums(vir)
vir <- vir[which(vir$Mn+vir$Al >= 10),]
vir <- vir[which(vir$Cu>=60&vir$Cu<=90),]
vir.elements <- c("Cu","Al","Ni","Mn","V")
# vir.elements <- c("Cu","Al","Ni","Mn","Zr")
vir <- vir[,vir.elements]
rownames(vir) <- NULL

vir.at <- data.frame(fn.at(vir,features[vir.elements,"amass"]))
vir.feature <- fn.vir.calFeature(dataReal = vir.at,features = features,elements = vir.elements,H=H)

vir.feature <- vir.feature[,-c(1:length(vir.elements))]

vir.feature <- fn.vir.maxmin.scale(vir.feature,dataFeature[,colnames(vir.feature)])
rownames(vir.feature) <- NULL

vir.Ms <- vir.feature[,gb.Ms.name]
# vir.Ms$Ms <- 0
vir.HMA <- vir.feature[,gb.HMA.name]
# vir.HMA$HMA <- 0

write.csv(vir,"vir-parallel-row.csv")
# 
# set.seed(111111)
# sp.num <- fn.vec.split(k=100,datasize = nrow(vir.Ms))

# library("doParallel")      
# library("foreach")
# cl<- makeCluster(detectCores()-7)      
# registerDoParallel(cl)      
# getDoParWorkers()  #
# time1 <- Sys.time()
# 
# vir.boots.Ms <- foreach(num=sp.num, 
#                         .combine = "cbind",
#                         .packages = c("xgboost")
# )%dopar% fn.pred.vir(data=data.Ms[num,],test = vir.Ms,boots=1000,
#                      formula = formula.f1,method = "xgb",xgb=c(3,50))
# 
# 
# time2 <- Sys.time()
# time2-time1
# 
# stopCluster(cl)
# gc()
# 
# 
# 
# 
# library("doParallel")  
# library("foreach")  
# cl<- makeCluster(detectCores()-7)      
# registerDoParallel(cl)
# getDoParWorkers()  
# time1 <- Sys.time()
# 
# vir.boots.HMA <- foreach(num = sp.num, 
#                          .combine = "cbind",
#                          .packages = c("xgboost")
# )%dopar% fn.boots.vir(data=data.HMA[num,],test = vir.HMA,boots = 1000,
#                      formula = formula.f2,method = "xgb",xgb=c(3,20))
# 
# 
# time2 <- Sys.time()
# time2-time1
# stopCluster(cl)
# gc(reset = T)




# Ms svr = c(1,1.6)
# HMA svr = c(1,1)

boots.Ms.num <- list()

for (b in 1:1000) {
  set.seed(1234+b)
  boots.Ms.num[[b]] <- sample(nrow(data.Ms),nrow(data.Ms),replace=T)
}


library("doParallel")      
library("foreach")
cl<- makeCluster(detectCores()-7)      
registerDoParallel(cl)      
getDoParWorkers()  #
time1 <- Sys.time()

vir.boots.Ms <- foreach(num=boots.Ms.num, 
                         .combine = "cbind",
                         .packages = c("e1071")
)%dopar% fn.pred.vir(data=data.Ms[num,],test = vir.Ms,
                      formula = formula.f1,method = "svr.rbf",svr = c(1,1.6))


time2 <- Sys.time()
time2-time1

stopCluster(cl)
gc()

# save.image("231113-CuAlMnNi-allboots.RData")

# save.image("231113-CuAlMnNiZr-allboots.RData")

vir.boots.Ms$mean <- rowMeans(vir.boots.Ms[,1:1000])
vir.boots.Ms$sd <- apply(vir.boots.Ms[,1:1000],1,sd)
vir.boots.Ms <- vir.boots.Ms[,-c(1:1000)]


# fn.pred.vir (data=data.Ms,test = vir.Ms[1:10000,],
#                 formula = formula.f1,method = "xgb",xgb=c(3,50))


boots.HMA.num <- list()

for (b in 1:1000) {
  set.seed(1234+b)
  boots.HMA.num[[b]] <- sample(nrow(data.HMA),nrow(data.HMA),replace=T)
}



library("doParallel")  
library("foreach")  
cl<- makeCluster(detectCores()-7)      
registerDoParallel(cl)
getDoParWorkers()  
time1 <- Sys.time()

vir.boots.HMA <- foreach(num = boots.HMA.num, 
                          .combine = "cbind",
                         .packages = c("e1071")
)%dopar% fn.pred.vir(data=data.HMA[num,],test = vir.HMA,
                      formula = formula.f2,method = "svr.rbf",svr = c(1,1))


time2 <- Sys.time()
time2-time1
stopCluster(cl)
gc(reset = T)

vir.boots.HMA$mean <- rowMeans(vir.boots.HMA[,1:1000])
vir.boots.HMA$sd <- apply(vir.boots.HMA[,1:1000],1,sd)

vir.boots.HMA <- vir.boots.HMA[,-c(1:1000)]

save.image("231113-CuAlMnNi-allboots.RData")


vir$ID <- c(1:nrow(vir))
vir$Ms.mean <- vir.boots.Ms$mean
vir$Ms.sd <- vir.boots.Ms$sd
vir$Ms.Prob <- fn.probability(mean = vir$Ms.mean,sd=vir$Ms.sd,up = 353,low = 333)
vir$Ms.Prob.sc <- fn.maxmin.scale.single(vir$Ms.Prob)
# vir$Ms.Prob <- log10(vir$Ms.Prob)
vir$HMA.mean <- vir.boots.HMA$mean
vir$HMA.sd <- vir.boots.HMA$sd
vir$HMA.PI <- fn.PI(mean = vir$HMA.mean,sd = vir$HMA.sd,best = max(data.HMA$HMA))
vir$HMA.EI <- fn.EI(mean=vir$HMA.mean,sd=vir$HMA.sd)
vir$HMA.EI.sc <- fn.maxmin.scale.single(vir$HMA.EI)
# vir$HMA.EI <- log10(vir$HMA.EI)
vir <- vir[,c("ID",vir.elements,"Ms.mean","Ms.sd","Ms.Prob","Ms.Prob.sc","HMA.mean","HMA.sd","HMA.PI","HMA.EI","HMA.EI.sc")]
# vir$prod <- vir$Ms.Prob*vir$HMA.EI
# vir <- vir[order(vir$prod,decreasing=T),]
# save.image("231106-CuAlMnNi.RData")

summary(vir$Ms.Prob)
summary(vir$HMA.EI)

sampling <- sample(nrow(vir),0.01*nrow(vir),replace = F)
write.csv(vir[sampling,c("Ms.mean","HMA.mean")],"sample.vir.csv")

## Ms.probability-HMA.EI
vir.PEI.pareto.num <- fn.paretoFront(f1=vir$Ms.Prob.sc,f2=vir$HMA.EI.sc,ID=vir$ID,"max-max")
vir.PEI.pareto <- vir[which(vir$ID %in% vir.PEI.pareto.num),]
# vir.PEI.pareto$HMA.EI <- abs(log10(vir.PEI.pareto$HMA.EI))
vir.PEI.pareto$product <- vir.PEI.pareto$Ms.Prob.sc*vir.PEI.pareto$HMA.EI.sc
vir.PEI.pareto$sum <- 0.5*vir.PEI.pareto$Ms.Prob.sc+0.5*vir.PEI.pareto$HMA.EI.sc
vir.PEI.pareto <- vir.PEI.pareto[order(vir.PEI.pareto$product,decreasing = T),]
vir.PEI.pareto <- vir.PEI.pareto[order(vir.PEI.pareto$sum,decreasing = T),]
vir.PEI.pareto <- vir.PEI.pareto[order(vir.PEI.pareto$cluster,decreasing = F),]
rownames(vir.PEI.pareto) <- NULL

library(factoextra)
library(cluster)
# PEI.km <- kmeans(vir.PEI.pareto[,c("Ms.Prob.sc","HMA.EI.sc")],centers=4,nstart = 25)
PEI.km <- kmeans(vir.PEI.pareto[,vir.elements],centers=4,nstart = 25)
fviz_cluster(PEI.km,data = vir.PEI.pareto[,c("Ms.Prob.sc","HMA.EI.sc")])

vir.PEI.pareto$cluster <- PEI.km$cluster
vir.PEI.pareto[which(vir.PEI.pareto$cluster==4),]

PEI.kmd <- pam(vir.PEI.pareto[,c("Ms.Prob","HMA.EI")],k = 4)
fviz_cluster(PEI.kmd,data = vir.PEI.pareto[,c("Ms.Prob","HMA.EI")])
# vir.PEI.pareto[,c("Ms.Prob","HMA.EI")]

PEI.kmd$medoids

PEI.candidate <- rbind(vir.PEI.pareto[which(vir.PEI.pareto$cluster==1),]
                       [which(vir.PEI.pareto[which(vir.PEI.pareto$cluster==1),]$product
                              ==max(vir.PEI.pareto[which(vir.PEI.pareto$cluster==1),]$product)),],
                       vir.PEI.pareto[which(vir.PEI.pareto$cluster==2),]
                       [which(vir.PEI.pareto[which(vir.PEI.pareto$cluster==2),]$product
                              ==max(vir.PEI.pareto[which(vir.PEI.pareto$cluster==2),]$product)),],
                       vir.PEI.pareto[which(vir.PEI.pareto$cluster==3),]
                       [which(vir.PEI.pareto[which(vir.PEI.pareto$cluster==3),]$product
                              ==max(vir.PEI.pareto[which(vir.PEI.pareto$cluster==3),]$product)),],
                       vir.PEI.pareto[which(vir.PEI.pareto$cluster==4),]
                       [which(vir.PEI.pareto[which(vir.PEI.pareto$cluster==4),]$product
                              ==max(vir.PEI.pareto[which(vir.PEI.pareto$cluster==4),]$product)),])

PEI.edge <- rbind(vir.PEI.pareto[which(vir.PEI.pareto$Ms.Prob.sc==1),],
                  vir.PEI.pareto[which(vir.PEI.pareto$HMA.EI.sc==1),])

vir.range <- vir[which(vir$Ms.mean >= 333 & vir$Ms.mean<=353),]
vir.range <- vir.range[order(vir.range$HMA.mean,decreasing = T),]

write.csv(vir.range[1:4,],"vir.range.csv")

library(topsis)
# d <- matrix(rpois(12, 5), nrow = 4)
d <- as.matrix(vir.PEI.pareto[,c("Ms.Prob","HMA.EI")])
w <- c(1, 1)
i <- c("+", "+")
topsis(d, w, i)

# nrow(vir.PEI.pareto)

vir.PEI.pareto[which()]

length(which(vir$Ms.Prob>=0.5))
length(which(vir$HMA.EI>=0.01))


# fn.probability(mean=360,sd=25.6,low=343,up=353)
# fn.probability(mean=349,sd=66,low=343,up=353)
# 
# x=c(seq((343-360)/25.6,(353-360)/25.6,0.01))
# plot(seq(-2,2,0.001),qnorm(seq(-2,2,0.001),log.p = T),xlim=c(-1,1),ylim=c(0,1))
# points(x,qnorm(x, log.p = T),xlim=c(-1,1),ylim=c(0,1))
# curve(qnorm(x), add = TRUE, col = "red", lwd = 2)

# vir.Msrange <-vir[which(vir$Ms.mean>=343&vir$Ms.mean<=353),] 
write.csv(PEI.candidate,"PEI.candidate1.csv")
write.csv(vir.PEI.pareto[1:4,],"PEI.selection1-allboots.csv")


plot.rand <- sample(nrow(vir),0.01*nrow(vir),replace = F)
plot(vir$Ms.Prob.sc[plot.rand],vir$HMA.EI.sc[plot.rand],
     xlim=c(0,1),ylim=c(0,1),
     pch=21,col="#00AFBB",bg=rgb(0,175,187,50,max=255),cex=1.5,lwd=1.5,main="Virtual Space",
     xlab="Ms.Probability",ylab="HMA.EI",
     cex.lab=1.2,tcl=0.4,cex.axis=1.2,font=2,font.lab=2,las=1)

points(vir.PEI.pareto$Ms.Prob.sc,vir.PEI.pareto$HMA.EI.sc,
       col="#CD1076",bg=rgb(205,16,118,180,max=255),pch=24,cex=2,lwd=1.5)
# abline(0,1,col="red")
# points(vir.PEI.pareto[c(1:4),]$Ms.Prob,vir.PEI.pareto[c(1:4),]$HMA.EI,
#        col="blue",bg=rgb(0,0,255,180,max=255),pch=24,cex=2,lwd=1.5)
# summary(vir.PEI.pareto$HMA.EI)
points(PEI.candidate$Ms.Prob.sc,PEI.candidate$HMA.EI.sc,
       col="blue",bg=rgb(0,0,255,180,max=255),pch=24,cex=2,lwd=1.5)

points(vir.PEI.pareto[1:4,]$Ms.Prob.sc,vir.PEI.pareto[1:4,]$HMA.EI.sc,
       col="blue",bg=rgb(0,0,255,180,max=255),pch=24,cex=2,lwd=1.5)



legend(x=0.8,y=1,legend=c("Dominated points","Pareto fronts","Selection"),
       pch=c(21, 24,24), col=c("#00AFBB", "#CD1076","blue"),pt.cex = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(0,175,187,50,max=255),rgb(205,16,118,180,max=255),rgb(0,0,255,180,max=255)),
       x.intersp = 1,y.intersp = 0.75)

axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.02)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.02)

box(lwd=3)


vir.mEI <- vir[which(vir$Ms.mean>=343&vir$Ms.mean<=353),]
vir.mEI <- vir.mEI[order(vir.mEI$HMA.EI,decreasing=T),]

write.csv(vir.mEI[1:4,vir.elements],"mEI.selection1.csv")

plot(vir.mEI$HMA.EI[-c(1:4)],vir.mEI$Ms.mean[-c(1:4)],
     xlim=c(0,0.4),ylim=c(340,357),
     pch=21,col="#00AFBB",bg=rgb(0,175,187,50,max=255),cex=1.5,lwd=1.5,main="Virtual Space",
     xlab="HMA.EI",ylab="Ms.Probability",
     cex.lab=1.2,tcl=0.4,cex.axis=1.2,font=2,font.lab=2,las=1)

points(vir.mEI$HMA.EI[c(1:4)],vir.mEI$Ms.mean[c(1:4)],
       col="#CD1076",bg=rgb(205,16,118,180,max=255),pch=24,cex=2,lwd=1.5)


legend(x=0.32,y=357,legend=c("Candidates","Selection"),
       pch=c(21, 24), col=c("#00AFBB", "#CD1076"),pt.cex = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(0,175,187,50,max=255),rgb(205,16,118,180,max=255)),x.intersp = 1,y.intersp = 0.75)

axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.02)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.02)

box(lwd=3)

# vir.PEI.pareto <- psel(vir,high(Ms.Prob)*high(HMA.EI))
# psel(vir,high(Ms.Prob)&high(HMA.EI))
# max(vir.PEI.pareto$Ms.Prob*vir.PEI.pareto$HMA.EI)
# vir.PEI.scale <- fn.maxmin.scale(vir.PEI.pareto[,c("Ms.Prob","HMA.EI")])
# vir.PEI.scale$product <- vir.PEI.scale$Ms.Prob*vir.PEI.scale$HMA.EI
# vir.PEI.scale[order(vir.PEI.scale$product,decreasing = T),]

## Ms.probability-HMA.PI
vir.PPI.pareto.num <- fn.paretoFront(f1=vir$Ms.Prob,f2=vir$HMA.PI,ID=vir$ID,"max-max")
vir.PPI.pareto <- vir[which(vir$ID %in% vir.PPI.pareto.num),]
vir.PPI.pareto$product <- vir.PPI.pareto$Ms.Prob*vir.PPI.pareto$HMA.PI
vir.PPI.pareto <- vir.PPI.pareto[order(vir.PPI.pareto$product,decreasing = T),]

plot.rand <- sample(nrow(vir),nrow(vir),replace = F)
plot(vir$Ms.Prob[plot.rand],vir$HMA.PI[plot.rand])
# points(vir$Ms.Prob[vir.PPI.pareto.num],vir$HMA.PI[vir.PPI.pareto.num],col="blue",cex=1.5,pch=16)
points(vir.PPI.pareto$Ms.Prob,vir.PPI.pareto$HMA.PI,col="red",cex=1.5,pch=16)
points(vir.PPI.pareto[1:4,]$Ms.Prob,vir.PPI.pareto[1:4,]$HMA.PI,col="purple",cex=1.5,pch=16)

## Ms.mean-HMA.EI
temp.up <- 353
temp.low <- 343
vir$Ms.dist <- abs(vir$Ms.mean-(353+343)/2)
vir.Msrange.num <- which(vir$Ms.mean>=343&vir$Ms.mean<=353)

vir.mEI.pareto.num <- fn.paretoFront(f1=vir$Ms.dist,f2=vir$HMA.EI,ID=vir$ID,"min-max")
vir.mEI.pareto <- vir[which(vir$ID %in% vir.mEI.pareto.num),]
vir.mEI.pareto <- vir.mEI.pareto[order(vir.mEI.pareto$HMA.EI,decreasing = T),]

plot.rand <- sample(nrow(vir),nrow(vir),replace = F)
plot(vir$Ms.dist[plot.rand],vir$HMA.EI[plot.rand])
# points(vir$Ms.Prob[vir.mEI.pareto.num],vir$HMA.PI[vir.mEI.pareto.num],col="blue",cex=1.5,pch=16)
points(vir.mEI.pareto$Ms.dist,vir.mEI.pareto$HMA.EI,col="red",cex=1.5,pch=16)
# points(vir.mEI.pareto[1:4,]$Ms.Prob,vir.mEI.pareto[1:4,]$HMA.PI,col="purple",cex=1.5,pch=16)

plot(vir$Ms.mean[-vir.Msrange.num],vir$HMA.EI[-vir.Msrange.num])
points(vir$Ms.mean[vir.Msrange.num],vir$HMA.EI[vir.Msrange.num],col="red")

## Ms.mean-HMA.mean

vir.mm.pareto.num <- fn.paretoFront(f1=vir$Ms.dist,f2=vir$HMA.mean,ID=vir$ID,"min-max")
vir.mm.pareto <- vir[which(vir$ID %in% vir.mm.pareto.num),]
vir.mm.pareto <- vir.mm.pareto[order(vir.mm.pareto$HMA.mean,decreasing = T),]

plot.rand <- sample(nrow(vir),nrow(vir),replace = F)
plot(vir$Ms.dist[plot.rand],vir$HMA.mean[plot.rand])
# points(vir$Ms.Prob[vir.mm.pareto.num],vir$HMA.PI[vir.mm.pareto.num],col="blue",cex=1.5,pch=16)
points(vir.mm.pareto$Ms.dist,vir.mm.pareto$HMA.mean,col="red",cex=1.5,pch=16)
points(vir.mm.pareto[1:4,]$Ms.Prob,vir.mm.pareto[1:4,]$HMA.PI,col="purple",cex=1.5,pch=16)


## Ms.probability-HMA.mean

vir.Pm.pareto.num <- fn.paretoFront(f1=vir$Ms.Prob,f2=vir$HMA.mean,ID=vir$ID,"max-max")
vir.Pm.pareto <- vir[which(vir$ID %in% vir.Pm.pareto.num),]
vir.Pm.pareto$product <- vir.Pm.pareto$Ms.Prob*vir.Pm.pareto$HMA.mean
vir.Pm.pareto <- vir.Pm.pareto[order(vir.Pm.pareto$product,decreasing = T),]

plot.rand <- sample(nrow(vir),nrow(vir),replace = F)
plot(vir$Ms.Prob[plot.rand],vir$HMA.mean[plot.rand])
# points(vir$Ms.Prob[vir.Pm.pareto.num],vir$HMA.PI[vir.Pm.pareto.num],col="blue",cex=1.5,pch=16)
points(vir.Pm.pareto$Ms.Prob,vir.Pm.pareto$HMA.mean,col="red",cex=1.5,pch=16)
points(vir.Pm.pareto[1:4,]$Ms.Prob,vir.Pm.pareto[1:4,]$HMA.mean,col="purple",cex=1.5,pch=16)
