
### different seed
time1<-Sys.time()
# pop.elements <- c("Ni","Co","Fe","Cu","Pd","Ti","Hf","Zr")
pop.elements <- c("Ni","Cu","Pd","Ti","Hf","Zr")
optimization <- list()
pareto.collection <- list()
seed.num <- 1
i <- 1
repeat{
  
  optimization[[i]] <- fn.NSGA2.prt(generation = 300,i.seed = i,b.num=1000,p.size=300,n.mp=150,m.prob=0.7,
                                    pareto=raw.pareto,data.hys,data.enth,fit.hys="hys.LCB",fit.enth="enth.LCB",
                                    dataFeature,features,pop.elements,H,best.hys=10,best.enth=-20,
                                    gb.hys.name,gb.enth.name,formula.f1,formula.f2,para.hys=1000,para.enth=950)

  pareto.collection[[i]] <- optimization[[i]][[300]]
  print(paste("iteration",i,"finished."))
  
  i <- i+1
  if(i > seed.num){
    break
  }
  
}
time2<-Sys.time()
NSGA.time1 <- time2-time1
print(NSGA.time1)
save.image("230906.RData")




### hypervolume calculation
hypervolume.opt <- matrix(ncol=seed.num,nrow = 300)

for (i in 1:seed.num) {
  for (j in 1:300) {
    
    hypervolume.opt[j,i] <- fn.hypervolume.easy(optimization[[i]][[j]][,c("hys.LCB","enth.LCB")],c(50,0))
    
  }
}



## EI.pareto
time3 <- Sys.time()
seed.num <- 1
pop.elements <- c("Ni","Cu","Pd","Ti","Hf","Zr")
optimization.e <- list()
pareto.e.collection <- list()

i <- 1
repeat{
  
  optimization.e[[i]] <- fn.NSGA2.prt(generation = 300,i.seed = i,b.num=1000,p.size=300,n.mp=150,m.prob=0.7,
                                      pareto=raw.pareto,data.hys,data.enth,fit.hys="hys.EI",fit.enth="enth.EI",
                                      dataFeature,features,pop.elements,H,best.hys=15,best.enth=-21.09,
                                      gb.hys.name,gb.enth.name,formula.f1,formula.f2,para.hys=1000,para.enth=850)
  
  pareto.e.collection[[i]] <- optimization.e[[i]][[300]]
  print(paste("iteration",i,"finished."))
  
  i <- i+1
  if(i > seed.num){
    break
  }
  
}


time4 <- Sys.time()
NSGA.time2 <- time4-time3
print(NSGA.time2)
save.image("230716.RData")


optimization.test <- fn.NSGA2.prt(generation = 300,i.seed = i,b.num=1000,p.size=300,n.mp=150,m.prob=0.7,
                                  pareto=raw.pareto,data.hys,data.enth,fit.hys="hys.mean",fit.enth="enth.mean",
                                  dataFeature,features,pop.elements,H,best.hys=15,best.enth=-21.09,
                                  gb.hys.name,gb.enth.name,formula.f1,formula.f2,para.hys=1000,para.enth=850)
save.image("230715.RData")

rowSums(pareto.test.all[,1:8])
rowSums(rawdata[,2:11])
rowSums(raw.pareto[,pop.elements]*10)

pareto.test.all <- optimization.test[[300]]
pareto.test.all <- pareto.test.all[!duplicated(pareto.test.all[,1:8]),]
rownames(pareto.test.all) <- NULL
pareto.test.all$uncertainty <- 4*pareto.test.all$hys.sd*pareto.test.all$enth.sd
pareto.test.all <- pareto.test.all[order(pareto.test.all$uncertainty,decreasing = T),]
pareto.test.all$distance <- fn.distance(f1=pareto.test.all$hys.mean,f2=pareto.test.all$enth.mean,target = c(0,-40))
plot(pareto.test.all$hys.mean,pareto.test.all$enth.mean,xlim=c(0,60),ylim=c(-40,0))
points(raw.pareto$hysteresis,raw.pareto$enthalpy,col="red")
points(pareto.m.all$hys.mean,pareto.m.all$enth.mean,col="blue")

raw.pareto[,pop.elements]*10
fn.initial(size=50)

parent <- rbind(pareto,initial)

plot(pareto.m.all$hys.mean,pareto.m.all$enth.mean,xlim=c(0,60),ylim=c(-40,0))
points(raw.pareto$hysteresis,raw.pareto$enthalpy,col="red")
sum(duplicated(rbind(raw.pareto[,pop.elements]*10,pareto.test.all[,pop.elements])))





## mean pareto
time5 <- Sys.time()
seed.num <- 1
pop.elements <- c("Ni","Co","Fe","Cu","Pd","Ti","Hf","Zr")
optimization.m <- list()
pareto.m.collection <- list()

i <- 1
repeat{
  
  optimization.m[[i]] <- fn.NSGA2.prt(generation = 300,i.seed = i,b.num=1000,p.size=300,n.mp=150,m.prob=0.7,
                                      pareto=raw.pareto,data.hys,data.enth,fit.hys="hys.mean",fit.enth="enth.mean",
                                      dataFeature,features,pop.elements,H,best.hys=15,best.enth=-21.09,
                                      gb.hys.name,gb.enth.name,formula.f1,formula.f2,para.hys=1000,para.enth=850)
  
  pareto.m.collection[[i]] <- optimization.m[[i]][[300]]
  print(paste("iteration",i,"finished."))
  
  i <- i+1
  if(i > seed.num){
    break
  }
  
}


time6 <- Sys.time()
NSGA.time3 <- time6-time5
print(NSGA.time2)
save.image("230716.RData")



hypervolume.opt.m <- matrix(ncol=5,nrow = 300)

for (i in 1:3) {
  for (j in 1:300) {
    
    hypervolume.opt.m[j,i] <- fn.hypervolume.easy(optimization.m[[i]][[j]][,c("hys.mean","enth.mean")],c(50,0))
    
  }
}



pareto.collection[[1]] <- optimization[[1]][[300]][order(optimization[[1]][[300]]$hys.LCB,decreasing=T),]
pareto.collection[[2]] <- optimization[[2]][[300]][order(optimization[[2]][[300]]$hys.LCB,decreasing=T),]
pareto.collection[[3]] <- optimization[[3]][[300]][order(optimization[[3]][[300]]$hys.LCB,decreasing=T),]
# pareto.collection[[4]] <- optimization[[4]][[300]][order(optimization[[4]][[300]]$hys.LCB,decreasing=T),]
# pareto.collection[[5]] <- optimization[[5]][[300]][order(optimization[[5]][[300]]$hys.LCB,decreasing=T),]

# pareto.collection[[1]] <- optimization[[1]][[300]][order(optimization[[1]][[300]]$hys.mean,decreasing=T),]
# pareto.collection[[2]] <- optimization[[2]][[300]][order(optimization[[2]][[300]]$hys.mean,decreasing=T),]
# pareto.collection[[3]] <- optimization[[3]][[300]][order(optimization[[3]][[300]]$hys.mean,decreasing=T),]
# pareto.collection[[4]] <- pareto.collection[[4]][order(pareto.collection[[4]]$hys.mean,decreasing=T),]
# pareto.collection[[5]] <- pareto.collection[[5]][order(pareto.collection[[5]]$hys.mean,decreasing=T),]




### LCB space pareto
# pareto.all <- rbind(pareto.collection[[1]],pareto.collection[[2]],pareto.collection[[3]])
pareto.all <- pareto.collection[[1]]
pareto.all <- pareto.all[!duplicated(pareto.all[,1:8]),]
rownames(pareto.all) <- NULL
pareto.all$uncertainty <- 4*pareto.all$hys.sd*pareto.all$enth.sd
pareto.all <- pareto.all[order(pareto.all$uncertainty,decreasing = T),]
pareto.all$distance <- fn.distance(f1=pareto.all$hys.mean,f2=pareto.all$enth.mean,target = c(0,-40))
pareto.all <- pareto.all[order(pareto.all$distance,decreasing = F),]
pareto.all$EI.product <- pareto.all$hys.EI*pareto.all$enth.EI
pareto.all <- pareto.all[order(pareto.all$EI.product,decreasing = T),]




LCB.num <- fn.paretoFront(f1=pareto.all$uncertainty,f2=pareto.all$distance,ID=c(1:nrow(pareto.all)),method = "max-min")
pareto.all[LCB.num,]

plot(pareto.all$uncertainty,pareto.all$distance)
points(pareto.all$uncertainty[LCB.num],pareto.all$distance[LCB.num],col="red")


pareto.all[which(pareto.all$distance==min(pareto.all$distance)),]


# pareto.all[which(pareto.all$distance==min(pareto.all$distance)),]
# write.csv(cbind(pareto.all[1:10,1:8]/10,pareto.all[1:10,9:16]),"pareto.all.unc.csv")
# write.csv(cbind(pareto.all[which(pareto.all$distance==min(pareto.all$distance)),1:8]/10,
#                 pareto.all[which(pareto.all$distance==min(pareto.all$distance)),9:16]), 
#           "pareto.all.dist.csv")
## pareto plot
par(mfrow=c(1,2))
plot(x=pareto.collection[[1]]$hys.LCB,y=pareto.collection[[1]]$enth.LCB,
     xlim=c(-5,50),ylim=c(-40,0),col="#248a8a",type="S",lwd=2,main="LCB space",
     xlab="Hysetresis LCB",ylab="Enthalpy LCB",tcl=0.4,cex.axis=1.2,font=2,cex.lab=1.2,font.lab=2,las=1)
points(x=pareto.collection[[1]]$hys.LCB,y=pareto.collection[[1]]$enth.LCB,
       col="#248a8a",pch=21,bg=rgb(36,138,138,100,max=255),cex=1.5,lwd=1.5)

lines(x=pareto.collection[[2]]$hys.LCB,y=pareto.collection[[2]]$enth.LCB,
      col="#16bfbf",type="S",lwd=1)
points(x=pareto.collection[[2]]$hys.LCB,y=pareto.collection[[2]]$enth.LCB,
       col="#16bfbf",pch=21,bg=rgb(22,191,191,100,max=255),cex=1.5,lwd=1.5)

lines(x=pareto.collection[[3]]$hys.LCB,y=pareto.collection[[3]]$enth.LCB,
      col="#a0e5e3",type="S",lwd=1)
points(x=pareto.collection[[3]]$hys.LCB,y=pareto.collection[[3]]$enth.LCB,
       col="#a0e5e3",pch=21,bg=rgb(160,229,227,100,max=255),cex=1.5,lwd=1.5)

points(x=candidate.LCB.unc[1:4,]$hys.LCB,y=candidate.LCB.unc[1:4,]$enth.LCB,
       col="purple",pch=23,bg=rgb(128,0,128,100,max=255),cex=2,lwd=2)
points(x=candidate.LCBd.unc[1:4,]$hys.LCB,y=candidate.LCBd.unc[1:4,]$enth.LCB,
       col="blue",pch=22,bg=rgb(0,0,255,100,max=255),cex=2,lwd=2)

# lines(x=pareto.collection[[4]]$hys.LCB,y=pareto.collection[[4]]$enth.LCB,
#       col="#d2f2ed",type="S",lwd=1)
# points(x=pareto.collection[[4]]$hys.LCB,y=pareto.collection[[4]]$enth.LCB,
#        col="#d2f2ed",pch=21,bg=rgb(210,242,237,100,max=255),cex=1.5,lwd=1.5)
# lines(x=pareto.collection[[5]]$hys.LCB,y=pareto.collection[[5]]$enth.LCB,
#       col="#238080",type="S",lwd=1)
# points(x=pareto.collection[[5]]$hys.LCB,y=pareto.collection[[5]]$enth.LCB,
#        col="#238080",pch=21,bg=rgb(35,128,128,100,max=255),cex=1.5,lwd=1.5)
# points(x=pareto.all[1:4,]$hys.LCB,y=pareto.all[1:4,]$enth.LCB,
#        col="purple",pch=24,bg=rgb(128,0,128,100,max=255),cex=2,lwd=2)

lines(x=LCB.pareto$hys.LCB,y=LCB.pareto$enth.LCB,
      col="#CD1076",type="S",lwd=2)
points(x=LCB.pareto$hys.LCB,y=LCB.pareto$enth.LCB,
       col="#CD1076",pch=24,bg=rgb(205,16,118,100,max=255),cex=1.5)
axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.03)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.03)
box(lwd=3)

# legend(x="topright",inset=.03,legend=c("NSGA2","training","selected"),
#        lty = c(1,1,NA),pch =c(21,21,24), col=c("#248a8a", "#CD1076","purple"),lwd = 2,bty="n",text.font = 2,
#        pt.bg=c(rgb(36,138,138,100,100,max=255),rgb(205,16,118,100,max=255),rgb(128,0,128,100,max=255)))
legend(x=30,y=-5,legend=c("NSGA2","training"),
       lty = c(1,1),pch =c(21,24), col=c("#248a8a", "#CD1076"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(36,138,138,100,100,max=255),rgb(205,16,118,100,max=255)))


### LCB space hypervolume
plot(x=1:300,y=hypervolume.opt[,1],type="l",xlim=c(1,300),ylim=c(950,1600),lwd=2.5,mgp=c(3,0.5,0),
     xlab="Generations",ylab="Hypervolume",
     tcl=0.4,cex.axis=1.2,font=2,font.lab=2,cex.lab=1.2,las=1,col="#248a8a")
points(x=1:300,y=hypervolume.opt[,2],type="l",col="#16bfbf",lwd=2.5)
points(x=1:300,y=hypervolume.opt[,3],type="l",col="#a0e5e3",lwd=2.5)
# points(x=1:300,y=hypervolume.opt[,4],type="l",col="#d2f2ed",lwd=2.5)
# points(x=1:300,y=hypervolume.opt[,5],type="l",col="#238080",lwd=2.5)
lines(x=1:300,y=rep(hypervolume.LCB,300),col="#CD1076",lwd=2.5,lty=3)
title(main="Hypervolume LCB",font=2,font.lab=2,cex=2)
axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.03)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.03)
box(lwd=3)
legend(x=160,y=1250,legend=c("seed1","seed2","seed3","seed4","seed5","training"),
       lty = c(1,1,1,1,1,3), col=c("#248a8a","#16bfbf", "#a0e5e3","#d2f2ed","#238080","#CD1076"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(0,175,187,50,max=255),rgb(205,16,118,180,max=255)))

par(mfrow=c(1,1))


library(factoextra)

center.num <- 3
km.LCB <- kmeans(pareto.all[,c("hys.LCB","enth.LCB")],centers = center.num,nstart = 24)
# km <- kmeans(pareto.m.all[,c("hys.mean","enth.mean")],centers = center.num,nstart = 24)
pareto.LCB.list <- list()

for (i in 1:center.num) {
  # pareto.mean.list[[i]] <- pareto.m.all[which(km[[1]]==i),]
  pareto.LCB.list[[i]] <- pareto.all[which(km.LCB[[1]]==i),]
}
candidate.LCB.unc <- NA
for (i in 1:center.num) {
  candidate.LCB.unc <- rbind(candidate.LCB.unc,
                     pareto.LCB.list[[i]][pareto.LCB.list[[i]]$uncertainty==max(pareto.LCB.list[[i]]$uncertainty),])
}
candidate.LCB.unc <- candidate.LCB.unc[-1,]

fviz_cluster(km.LCB,pareto.all[,c("hys.LCB","enth.LCB")])

write.csv(cbind(candidate.LCB.unc[,1:8]/10,candidate.LCB.unc[,9:18]),"candidate.LCB.unc1.csv")



pareto.LCBd.list <- list()

for (i in 1:center.num) {
  # pareto.mean.list[[i]] <- pareto.m.all[which(km[[1]]==i),]
  pareto.LCBd.list[[i]] <- pareto.all[which(km.LCB[[1]]==i),]
}
candidate.LCB.dist <- NA
for (i in 1:center.num) {
  candidate.LCB.dist <- rbind(candidate.LCB.dist,
                             pareto.LCBd.list[[i]][pareto.LCBd.list[[i]]$distance==min(pareto.LCBd.list[[i]]$distance),])
}
candidate.LCB.dist <- candidate.LCB.dist[-1,]

fviz_cluster(km.LCB,pareto.all[,c("hys.LCB","enth.LCB")])
write.csv(cbind(candidate.LCB.dist[,1:8]/10,candidate.LCB.dist[,9:18]),"candidate.LCB.dist1.csv")
# property.pareto <- property.pareto[order(property.pareto$hysteresis,decreasing = T),]

## pareto plot in mean
par(mfrow=c(1,2))
# plot(x=pareto.collection[[1]]$hys.mean,y=pareto.collection[[1]]$enth.mean,
#      xlim=c(-5,50),ylim=c(-40,0),col="#248a8a",type="S",lwd=2,main="LCB space",
#      xlab="Hysetresis LCB",ylab="Enthalpy LCB",tcl=0.4,cex.axis=1.2,font=2,cex.lab=1.2,font.lab=2,las=1)
# points(x=pareto.collection[[1]]$hys.mean,y=pareto.collection[[1]]$enth.mean,
#        col="#248a8a",pch=21,bg=rgb(36,138,138,100,max=255),cex=1.5,lwd=1.5)

plot(x=pareto.collection[[1]]$hys.mean,y=pareto.collection[[1]]$enth.mean,
     xlim=c(-5,55),ylim=c(-40,0),col="#248a8a",pch=21,bg=rgb(36,138,138,100,max=255),cex=1.5,lwd=1.5,
     main="mean space",xlab="Hysetresis mean",ylab="Enthalpy mean",tcl=0.4,cex.axis=1.2,
     font=2,cex.lab=1.2,font.lab=2,las=1)

# lines(x=pareto.collection[[2]]$hys.mean,y=pareto.collection[[2]]$enth.mean,
#       col="#16bfbf",type="S",lwd=1)
points(x=pareto.collection[[2]]$hys.mean,y=pareto.collection[[2]]$enth.mean,
       col="#16bfbf",pch=21,bg=rgb(22,191,191,100,max=255),cex=1.5,lwd=1.5)

# lines(x=pareto.collection[[3]]$hys.mean,y=pareto.collection[[3]]$enth.mean,
#       col="#a0e5e3",type="S",lwd=1)
points(x=pareto.collection[[3]]$hys.mean,y=pareto.collection[[3]]$enth.mean,
       col="#a0e5e3",pch=21,bg=rgb(160,229,227,100,max=255),cex=1.5,lwd=1.5)

points(x=candidate.LCB.unc[1:4,]$hys.mean,y=candidate.LCB.unc[1:4,]$enth.mean,
       col="purple",pch=23,bg=rgb(128,0,128,100,max=255),cex=2,lwd=2)
points(x=candidate.LCB.dist[1:4,]$hys.mean,y=candidate.LCB.dist[1:4,]$enth.mean,
       col="blue",pch=22,bg=rgb(0,0,255,100,max=255),cex=2,lwd=2)

# lines(x=pareto.collection[[4]]$hys.LCB,y=pareto.collection[[4]]$enth.LCB,
#       col="#d2f2ed",type="S",lwd=1)
# points(x=pareto.collection[[4]]$hys.LCB,y=pareto.collection[[4]]$enth.LCB,
#        col="#d2f2ed",pch=21,bg=rgb(210,242,237,100,max=255),cex=1.5,lwd=1.5)
# lines(x=pareto.collection[[5]]$hys.LCB,y=pareto.collection[[5]]$enth.LCB,
#       col="#238080",type="S",lwd=1)
# points(x=pareto.collection[[5]]$hys.LCB,y=pareto.collection[[5]]$enth.LCB,
#        col="#238080",pch=21,bg=rgb(35,128,128,100,max=255),cex=1.5,lwd=1.5)
# points(x=pareto.all[1:4,]$hys.LCB,y=pareto.all[1:4,]$enth.LCB,
#        col="purple",pch=24,bg=rgb(128,0,128,100,max=255),cex=2,lwd=2)

lines(x=property.pareto$hysteresis,y=property.pareto$enthalpy,
      col="#CD1076",type="S",lwd=2)
points(x=property.pareto$hysteresis,y=property.pareto$enthalpy,
       col="#CD1076",pch=24,bg=rgb(205,16,118,100,max=255),cex=1.5)

# lines(x=LCB.pareto$hys.LCB,y=LCB.pareto$enth.LCB,
#       col="#CD1076",type="S",lwd=2)
# points(x=LCB.pareto$hys.LCB,y=LCB.pareto$enth.LCB,
#        col="#CD1076",pch=24,bg=rgb(205,16,118,100,max=255),cex=1.5)
axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.03)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.03)
box(lwd=3)

# legend(x="topright",inset=.03,legend=c("NSGA2","training","selected"),
#        lty = c(1,1,NA),pch =c(21,21,24), col=c("#248a8a", "#CD1076","purple"),lwd = 2,bty="n",text.font = 2,
#        pt.bg=c(rgb(36,138,138,100,100,max=255),rgb(205,16,118,100,max=255),rgb(128,0,128,100,max=255)))
legend(x=30,y=-5,legend=c("NSGA2","training"),
       lty = c(1,1),pch =c(21,24), col=c("#248a8a", "#CD1076"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(36,138,138,100,100,max=255),rgb(205,16,118,100,max=255)))


### LCB space hypervolume
plot(x=1:300,y=hypervolume.opt[,1],type="l",xlim=c(1,300),ylim=c(950,1600),lwd=2.5,mgp=c(3,0.5,0),
     xlab="Generations",ylab="Hypervolume",
     tcl=0.4,cex.axis=1.2,font=2,font.lab=2,cex.lab=1.2,las=1,col="#248a8a")
points(x=1:300,y=hypervolume.opt[,2],type="l",col="#16bfbf",lwd=2.5)
points(x=1:300,y=hypervolume.opt[,3],type="l",col="#a0e5e3",lwd=2.5)
# points(x=1:300,y=hypervolume.opt[,4],type="l",col="#d2f2ed",lwd=2.5)
# points(x=1:300,y=hypervolume.opt[,5],type="l",col="#238080",lwd=2.5)
lines(x=1:300,y=rep(hypervolume.LCB,300),col="#CD1076",lwd=2.5,lty=3)
title(main="Hypervolume LCB",font=2,font.lab=2,cex=2)
axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.03)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.03)
box(lwd=3)
legend(x=160,y=1250,legend=c("seed1","seed2","seed3","seed4","seed5","training"),
       lty = c(1,1,1,1,1,3), col=c("#248a8a","#16bfbf", "#a0e5e3","#d2f2ed","#238080","#CD1076"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(0,175,187,50,max=255),rgb(205,16,118,180,max=255)))

par(mfrow=c(1,1))



# pareto.m.all[which(km[[1]]==1),c("hys.mean","enth.mean")]

plot(x=pareto.all[which(km[[1]]==1),c("hys.LCB")],y=pareto.all[which(km[[1]]==1),c("enth.LCB")],
     xlim = c(0,50),ylim=c(-40,0))
points(x=pareto.all[which(km[[1]]==2),c("hys.LCB")],y=pareto.all[which(km[[1]]==2),c("enth.LCB")],col="red")
points(x=pareto.all[which(km[[1]]==3),c("hys.LCB")],y=pareto.all[which(km[[1]]==3),c("enth.LCB")],col="blue")

# fviz_cluster(km,pareto.all[,c("hys.LCB","enth.LCB")])


# pareto.all.num <-fn.paretoFront(f1=pareto.all$hys.LCB,f2=pareto.all$enth.LCB,ID=c(1:nrow(pareto.all)))
# 
# new.pareto.all <- pareto.all[pareto.all.num,]
# 
# new.pareto.all$uncertainty <- 4*new.pareto.all$hys.sd*new.pareto.all$enth.sd


# new.pareto.all <- new.pareto.all[order(new.pareto.all$uncertainty,decreasing = T),]

pareto.m.collection[[1]] <- pareto.m.collection[[1]][order(pareto.m.collection[[1]]$hys.mean,decreasing=T),]
pareto.m.collection[[2]] <- pareto.m.collection[[2]][order(pareto.m.collection[[2]]$hys.mean,decreasing=T),]
pareto.m.collection[[3]] <- pareto.m.collection[[3]][order(pareto.m.collection[[3]]$hys.mean,decreasing=T),]
# pareto.m.collection[[4]] <- pareto.m.collection[[4]][order(pareto.m.collection[[4]]$hys.mean,decreasing=T),]
# pareto.m.collection[[5]] <- pareto.m.collection[[5]][order(pareto.m.collection[[5]]$hys.mean,decreasing=T),]

## mean space pareto

# pareto.m.all <- rbind(pareto.m.collection[[1]],pareto.m.collection[[2]],pareto.m.collection[[3]])
pareto.m.all <- rbind(pareto.m.collection[[1]])
pareto.m.all <- pareto.m.all[!duplicated(pareto.m.all[,1:8]),]
pareto.m.all$uncertainty <- 4*pareto.m.all$hys.sd*pareto.m.all$enth.sd
pareto.m.all <- pareto.m.all[order(pareto.m.all$uncertainty,decreasing = T),]
pareto.m.all$distance <- fn.distance(f1=pareto.m.all$hys.mean,f2=pareto.m.all$enth.mean,target = c(0,-40))
rownames(pareto.m.all) <- NULL
pareto.m.all <- pareto.m.all[order(pareto.m.all$distance,decreasing = F),]
pareto.m.all[which(pareto.m.all$distance==min(pareto.m.all$distance)),]

pareto.m.all$EI.product <- pareto.m.all$hys.EI*pareto.m.all$enth.EI
pareto.m.all <- pareto.m.all[order(pareto.m.all$EI.product,decreasing = T),]
# write.csv(cbind(pareto.m.all[1:10,1:8]/10,pareto.m.all[1:10,9:16]),"pareto.m.all.unc.csv")
# write.csv(cbind(pareto.m.all[which(pareto.m.all$distance==min(pareto.m.all$distance)),1:8]/10,
#                 pareto.m.all[which(pareto.m.all$distance==min(pareto.m.all$distance)),9:16]),
#           "pareto.m.all.dist.csv")

mean.num <- fn.paretoFront(f1=pareto.m.all$uncertainty,f2=pareto.m.all$distance,ID=c(1:nrow(pareto.m.all)),method = "max-min")
mean.pareto <- pareto.m.all[mean.num,]

plot(mean.pareto$uncertainty,mean.pareto$distance)
points(pareto.mean.list[[1]]$uncertainty,pareto.mean.list[[1]]$distance,col="red",cex=1.5)
points(pareto.mean.list[[2]]$uncertainty,pareto.mean.list[[2]]$distance,col="blue",cex=1.5)
points(pareto.mean.list[[3]]$uncertainty,pareto.mean.list[[3]]$distance,col="darkgreen",cex=1.5)



plot(pareto.all$hys.mean[1:3],pareto.all$enth.mean[1:3],xlim=c(0,60),ylim=c(-50,0),pch=16,cex=1.5)
points(pareto.m.all$hys.mean[1:3],pareto.m.all$enth.mean[1:3],col="red",pch=16,cex=1.5)
points(pareto.e.all$hys.mean[1:3],pareto.e.all$enth.mean[1:3],col="blue",pch=16,cex=1.5)
points(raw.pareto$hysteresis,raw.pareto$enthalpy,col="darkgreen")

combination.mr <- rbind(pareto.m.all[,c(pop.elements)],
                        raw.pareto[,c(pop.elements)])
combination.mr$hysteresis <- c(pareto.m.all$hys.mean,raw.pareto$hysteresis)
combination.mr$enthalpy <- c(pareto.m.all$enth.mean,raw.pareto$enthalpy)
combination.mr$ID <- c(1:nrow(combination.mr))
rownames(combination.mr) <- NULL
comb.num <- fn.paretoFront(f1=combination.mr$hysteresis,f2=combination.mr$enthalpy,ID=combination.mr$ID)
comb.mr.pareto <- combination.mr[comb.num,]


write.csv(cbind(pareto.all[1:3,1:8]/10,pareto.all[1:3,9:18]),"LCB-EIp-greedy.iter1.csv")
write.csv(cbind(pareto.m.all[1:3,1:8]/10,pareto.m.all[1:3,9:18]),"mean-EIp-greedy.iter1.csv")
write.csv(cbind(pareto.e.all[1:3,1:8]/10,pareto.m.all[1:3,9:18]),"EI-EIp-greedy.iter1.csv")



# library(dbscan)
# dbscan(mean.pareto, eps = .8, minPts = 5)




library(factoextra)

center.num <- 3
km.mean <- kmeans(mean.pareto[,c("uncertainty","distance")],centers = center.num,nstart = 10)
# km <- kmeans(pareto.m.all[,c("hys.mean","enth.mean")],centers = center.num,nstart = 24)
pareto.mean.list <- list()

for (i in 1:center.num) {
  # pareto.mean.list[[i]] <- pareto.m.all[which(km[[1]]==i),]
  pareto.mean.list[[i]] <- mean.pareto[which(km.mean[[1]]==i),]
}

candidate.mean.unc <- NA
for (i in 1:center.num) {
  candidate.mean.unc <- rbind(candidate.mean.unc,
                             pareto.mean.list[[i]][pareto.mean.list[[i]]$uncertainty==max(pareto.mean.list[[i]]$uncertainty),])
}
candidate.mean.unc <- candidate.mean.unc[-1,]

fviz_cluster(km.mean,mean.pareto[,c("uncertainty","distance")])

write.csv(cbind(candidate.mean.unc[,1:8]/10,candidate.mean.unc[,9:18]),"candidate.mean.unc1.csv")



pareto.meand.list <- list()

for (i in 1:center.num) {
  # pareto.mean.list[[i]] <- pareto.m.all[which(km[[1]]==i),]
  pareto.meand.list[[i]] <- pareto.m.all[which(km.mean[[1]]==i),]
  pareto.meand.list[[i]] <- pareto.meand.list[[i]][order(pareto.meand.list[[i]]$distance,decreasing = F),]
}
candidate.mean.dist <- NA
for (i in 1:center.num) {
  candidate.mean.dist <- rbind(candidate.mean.dist,
                              pareto.meand.list[[i]][pareto.meand.list[[i]]$distance==min(pareto.meand.list[[i]]$distance),])
  # candidate.mean.dist <- rbind(candidate.mean.dist,
  #                              pareto.meand.list[[i]])
}
candidate.mean.dist <- candidate.mean.dist[-1,]
# pareto.meand.list[[3]]


fviz_cluster(km.mean,pareto.m.all[,c("hys.mean","enth.mean")])
write.csv(cbind(candidate.mean.dist[,1:8]/10,candidate.mean.dist[,9:18]),"candidate.mean.dist1.csv")
# property.pareto <- property.pareto[order(property.pareto$hysteresis,decreasing = T),]



## EI pareto
pareto.e.collection[[1]] <- pareto.e.collection[[1]][order(pareto.e.collection[[1]]$hys.mean,decreasing=T),]
# pareto.e.collection[[2]] <- pareto.e.collection[[2]][order(pareto.e.collection[[2]]$hys.mean,decreasing=T),]
# pareto.e.collection[[3]] <- pareto.e.collection[[3]][order(pareto.e.collection[[3]]$hys.mean,decreasing=T),]


# pareto.e.all <- rbind(pareto.e.collection[[1]],pareto.e.collection[[2]],pareto.e.collection[[3]])
pareto.e.all <- pareto.e.collection[[1]]
pareto.e.all <- pareto.e.all[!duplicated(pareto.e.all[,1:8]),]
rownames(pareto.e.all) <- NULL
pareto.e.all$uncertainty <- 4*pareto.e.all$hys.sd*pareto.e.all$enth.sd
pareto.e.all <- pareto.e.all[order(pareto.e.all$uncertainty,decreasing = T),]
pareto.e.all$distance <- fn.distance(f1=pareto.e.all$hys.mean,f2=pareto.e.all$enth.mean,target = c(0,-40))
pareto.e.all <- pareto.e.all[order(pareto.e.all$distance,decreasing = F),]
pareto.e.all$EI.product <- pareto.e.all$hys.EI*pareto.e.all$enth.EI
pareto.e.all <- pareto.e.all[order(pareto.e.all$EI.product,decreasing = T),]
pareto.e.all[which(pareto.e.all$distance==min(pareto.e.all$distance)),]
pareto.e.all[which(pareto.e.all$EI.product==max(pareto.e.all$EI.product)),]

pareto.e.all <- pareto.e.all[order(pareto.e.all$hys.mean,decreasing = F),]


EI.num <- fn.paretoFront(f1=pareto.e.all$uncertainty,f2=pareto.e.all$distance,ID=c(1:nrow(pareto.e.all)),method = "max-min")
pareto.e.all[EI.num,]



plot(pareto.e.all$uncertainty,pareto.e.all$distance)
points(pareto.e.all$uncertainty[EI.num],pareto.e.all$distance[EI.num],col="red")







plot(pareto.m.all$hys.mean,pareto.m.all$enth.mean)
points(raw.pareto$hysteresis,raw.pareto$enthalpy,col="red")
plot(pareto.m.all$hys.EI,pareto.m.all$enth.EI)

plot(pareto.e.all$hys.EI,pareto.e.all$enth.EI,xlim=c(-0.15,0),ylim=c(0,-0.15))
plot(pareto.e.all$hys.mean,pareto.e.all$enth.mean)
points(raw.pareto$hysteresis,raw.pareto$enthalpy,col="red")




pareto.e.all[1:8]
# dataFeature,features,pop.elements,H,


set.seed(1)
EI.features <- fn.featured(dataFeature = dataFeature,pop = pareto.e.all[1:8],features,pop.elements,H)

EI.hys.fea <- EI.features[,gb.hys.name]
EI.enth.fea <- EI.features[,gb.enth.name]

boots.num.hys <- list()
for (j in 1:1000) {
  set.seed(10000+j)
  boots.num.hys[[j]] <- sample(nrow(data.hys),nrow(data.hys),replace = T)
}

boots.num.enth <- list()
for (j in 1:1000) {
  set.seed(10000+j)
  boots.num.enth[[j]] <- sample(nrow(data.enth),nrow(data.enth),replace = T)
}


rawdata[which(fn.distance(f1=rawdata$hysteresis,f2=rawdata$enthalpy,target=c(0,-40))==
        min(fn.distance(f1=rawdata$hysteresis,f2=rawdata$enthalpy,target=c(0,-40)))),]



EI.hys.test <- fn.par.hys(data=data.hys,test = EI.hys.fea,formula=formula.f1,times=1000,boots.num=boots.num.hys,para=950)
EI.hys.test <- data.frame(EI.hys.test)
EI.hys.test$hys.new.EI <- -fn.EI(mean=EI.hys.test[,"hys.mean"],sd=EI.hys.test[,"hys.sd"],property=15,max=F)

which(EI.hys.test$hys.EI==min(EI.hys.test$hys.EI))
which(EI.hys.test$hys.new.EI==min(EI.hys.test$hys.new.EI))

EI.hys.test[which(EI.hys.test$hys.new.EI<=-4),]

EI.enth.test <- fn.par.enth(data=data.enth,test = EI.enth.fea,formula=formula.f2,times=1000,boots.num=boots.num.enth,para=800)
EI.enth.test <- data.frame(EI.enth.test)
EI.enth.test$enth.new.EI <- -fn.EI(mean=EI.enth.test[,"enth.mean"],sd=EI.enth.test[,"enth.sd"],property=-21.09,max=F)

which(EI.enth.test$enth.EI==min(EI.enth.test$enth.EI))
which(EI.enth.test$enth.new.EI==min(EI.enth.test$enth.new.EI))
EI.enth.test[which(EI.enth.test$enth.new.EI<=-4),]
summary(EI.enth.test$enth.new.EI)
EI.results <- cbind(EI.hys.test[,c("hys.mean","hys.sd","hys.new.EI")],EI.enth.test[,c("enth.mean","enth.sd","enth.new.EI")])

plot(-EI.results$hys.new.EI,-EI.results$enth.new.EI)
plot(-pareto.e.all$hys.EI,-pareto.e.all$enth.EI)



library(factoextra)

center.num <- 3
# pareto.e.all[,c("hys.EI","enth.EI")] <- -log10(-pareto.e.all[,c("hys.EI","enth.EI")]) 
km.EI <- kmeans(pareto.e.all[,c("hys.EI","enth.EI")],centers = center.num,nstart = 24)

pareto.EI.list <- list()

for (i in 1:center.num) {
  # pareto.EI.list[[i]] <- pareto.e.all[which(km[[1]]==i),]
  pareto.EI.list[[i]] <- pareto.e.all[which(km.EI[[1]]==i),]
}
candidate.EI.unc <- NA
for (i in 1:center.num) {
  candidate.EI.unc <- rbind(candidate.EI.unc,
                              pareto.EI.list[[i]][pareto.EI.list[[i]]$uncertainty==max(pareto.EI.list[[i]]$uncertainty),])
}
candidate.EI.unc <- candidate.EI.unc[-1,]

fviz_cluster(km.EI,pareto.e.all[,c("hys.EI","enth.EI")])

write.csv(cbind(candidate.EI.unc[,1:8]/10,candidate.EI.unc[,9:18]),"candidate.EI.unc1.csv")



pareto.EId.list <- list()

for (i in 1:center.num) {
  pareto.EId.list[[i]] <- pareto.e.all[which(km.EI[[1]]==i),]
  pareto.EId.list[[i]] <- pareto.EId.list[[i]][order(pareto.EId.list[[i]]$distance,decreasing = F),]
}
candidate.EI.dist <- NA
for (i in 1:center.num) {
  candidate.EI.dist <- rbind(candidate.EI.dist,
                               pareto.EId.list[[i]][pareto.EId.list[[i]]$distance==min(pareto.EId.list[[i]]$distance),])
  # candidate.EI.dist <- rbind(candidate.EI.dist,
  #                              pareto.EId.list[[i]])
}
candidate.EI.dist <- candidate.EI.dist[-1,]




fviz_cluster(km.EI,pareto.e.all[,c("hys.EI","enth.EI")])
write.csv(cbind(candidate.EI.dist[,1:8]/10,candidate.EI.dist[,9:18]),"candidate.EI.dist1.csv")
# property.pareto <- property.pareto[order(property.pareto$hysteresis,decreasing = T),]










## pareto plot
par(mfrow=c(1,2))
plot(x=pareto.m.collection[[1]]$hys.mean,y=pareto.m.collection[[1]]$enth.mean,
     xlim=c(0,50),ylim=c(-40,0),col="#1C65A3",type="S",lwd=2,main="mean space",
     xlab="Hysetresis mean",ylab="Enthalpy mean",tcl=0.4,cex.axis=1.2,font=2,cex.lab=1.2,font.lab=2,las=1)
points(x=pareto.m.collection[[1]]$hys.mean,y=pareto.m.collection[[1]]$enth.mean,
       col="#1C65A3",pch=22,bg=rgb(28,101,163,100,max=255),cex=1.5,lwd=1.5)

lines(x=pareto.m.collection[[2]]$hys.mean,y=pareto.m.collection[[2]]$enth.mean,
      col="#1C6FAE",type="S",lwd=2)
points(x=pareto.m.collection[[2]]$hys.mean,y=pareto.m.collection[[2]]$enth.mean,
       col="#1C6FAE",pch=22,bg=rgb(28,111,174,100,max=255),cex=1.5)

lines(x=pareto.m.collection[[3]]$hys.mean,y=pareto.m.collection[[3]]$enth.mean,
      col="#4993C0",type="S",lwd=2)
points(x=pareto.m.collection[[3]]$hys.mean,y=pareto.m.collection[[3]]$enth.mean,
       col="#4993C0",pch=22,bg=rgb(73,147,192,100,max=255),cex=1.5)

points(x=candidate.mean.unc[1:3,]$hys.mean,y=candidate.mean.unc[1:3,]$enth.mean,
       col="purple",pch=23,bg=rgb(128,0,128,100,max=255),cex=2,lwd=2)
points(x=candidate.mean.dist[1:3,]$hys.LCB,y=candidate.mean.dist[1:3,]$enth.mean,
       col="blue",pch=22,bg=rgb(0,0,255,100,max=255),cex=2,lwd=2)


# lines(x=pareto.m.collection[[4]]$hys.mean,y=pareto.m.collection[[4]]$enth.mean,
#       col="#64AAD2",type="S",lwd=2)
# points(x=pareto.m.collection[[4]]$hys.mean,y=pareto.m.collection[[4]]$enth.mean,
#        col="#64AAD2",pch=22,bg=rgb(100,170,210,100,max=255),cex=1.5)
# lines(x=pareto.m.collection[[5]]$hys.mean,y=pareto.m.collection[[5]]$enth.mean,
#       col="#93CDDF",type="S",lwd=2)
# points(x=pareto.m.collection[[5]]$hys.mean,y=pareto.m.collection[[5]]$enth.mean,
#        col="#93CDDF",pch=22,bg=rgb(147,205,223,100,max=255),cex=1.5)
# points(x=pareto.m.all[1:4,]$hys.mean,y=pareto.m.all[1:4,]$enth.mean,
#        col="red",pch=24,bg=rgb(255,0,0,100,max=255),cex=2,lwd=2)

lines(x=mean.pareto$hys.mean,y=mean.pareto$enth.mean,
      col="#e91926",type="S",lwd=2)
points(x=mean.pareto$hys.mean,y=mean.pareto$enth.mean,
       col="#e91926",pch=22,bg=rgb(233,25,38,100,max=255),cex=1.5)
axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.02)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.02)
box(lwd=3)

legend(x="topright",inset=.03,legend=c("NSGA2","training","selected"),
       lty = c(1,1),pch =c(22,22), col=c("#1C65A3", "#e91926"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(28,101,163,100,max=255),rgb(233,25,38,100,max=255)))

### mean space hypervolume
plot(x=1:300,y=hypervolume.opt.m[,1],type="l",xlim=c(1,300),ylim=c(700,1400),lwd=2.5,mgp=c(3,0.5,0),main="Hypervolume mean",
     xlab="Generations",ylab="Hypervolume",tcl=0.4,cex.axis=1.2,font=2,font.lab=2,cex.lab=1.2,las=1,col="#1C65A3")
points(x=1:300,y=hypervolume.opt.m[,2],type="l",col="#1C6FAE",lwd=2.5)
points(x=1:300,y=hypervolume.opt.m[,3],type="l",col="#4993C0",lwd=2.5)
points(x=1:300,y=hypervolume.opt.m[,4],type="l",col="#64AAD2",lwd=2.5)
points(x=1:300,y=hypervolume.opt.m[,5],type="l",col="#93CDDF",lwd=2.5)
lines(x=1:300,y=rep(hypervolume.mean,300),col="#e91926",lwd=2.5,lty=3)

axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.03)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.03)
box(lwd=3)
legend(x="bottomright",inset=.03,legend=c("seed1","seed2","seed3","seed4","seed5","training"),
       lty = c(1,1,1,1,1,3), col=c("#1C65A3","#1C6FAE", "#4993C0","#64AAD2","#93CDDF","#e91926"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(0,175,187,50,max=255),rgb(205,16,118,180,max=255)))

par(mfrow=c(1,1))





## prediction space pareto
par(mfrow=c(1,2))
plot(x=pareto.p.collection[[1]]$hys.mean,y=pareto.p.collection[[1]]$enth.mean,
     xlim=c(0,50),ylim=c(-40,0),col="#36903E",type="S",lwd=2,main="predcition space",
     xlab="Hysetresis prediction",ylab="Enthalpy prediction",tcl=0.4,cex.axis=1.2,font=2,cex.lab=1.2,font.lab=2,las=1)
points(x=pareto.p.collection[[1]]$hys.mean,y=pareto.p.collection[[1]]$enth.mean,
       col="#36903E",pch=23,bg=rgb(54,144,62,100,max=255),cex=1.5,lwd=1.5)

lines(x=pareto.p.collection[[2]]$hys.mean,y=pareto.p.collection[[2]]$enth.mean,
      col="#43A245",type="S",lwd=2)
points(x=pareto.p.collection[[2]]$hys.mean,y=pareto.p.collection[[2]]$enth.mean,
       col="#43A245",pch=23,bg=rgb(67,162,69,100,max=255),cex=1.5)

lines(x=pareto.p.collection[[3]]$hys.mean,y=pareto.p.collection[[3]]$enth.mean,
      col="#61B05D",type="S",lwd=2)
points(x=pareto.p.collection[[3]]$hys.mean,y=pareto.p.collection[[3]]$enth.mean,
       col="#61B05D",pch=23,bg=rgb(97,176,93,100,max=255),cex=1.5)

lines(x=pareto.p.collection[[4]]$hys.mean,y=pareto.p.collection[[4]]$enth.mean,
      col="#87C47F",type="S",lwd=2)
points(x=pareto.p.collection[[4]]$hys.mean,y=pareto.p.collection[[4]]$enth.mean,
       col="#87C47F",pch=23,bg=rgb(135,196,127,100,max=255),cex=1.5)
lines(x=pareto.p.collection[[5]]$hys.mean,y=pareto.p.collection[[5]]$enth.mean,
      col="#A7D59E",type="S",lwd=2)
points(x=pareto.p.collection[[5]]$hys.mean,y=pareto.p.collection[[5]]$enth.mean,
       col="#A7D59E",pch=23,bg=rgb(167,213,158,100,max=255),cex=1.5)



lines(x=property.pareto$hysteresis,y=property.pareto$enthalpy,
      col="#90365b",type="S",lwd=2)
points(x=property.pareto$hysteresis,y=property.pareto$enthalpy,
       col="#90365b",pch=23,bg=rgb(144,54,91,100,max=255),cex=1.5)
axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.02)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.02)
box(lwd=3)

legend(x="topright",inset=.05,legend=c("NSGA2","Training"),
       lty = c(1,1),pch =c(23,23), col=c("#36903E", "#90365b"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(28,101,163,100,max=255),rgb(233,25,38,100,max=255)))


## hypervolume prediction
plot(x=1:300,y=hypervolume.opt.m[,1],type="l",xlim=c(1,300),ylim=c(700,1400),lwd=2.5,mgp=c(3,0.5,0),main="Hypervolume prediction",
     xlab="Generations",ylab="Hypervolume",tcl=0.4,cex.axis=1.2,font=2,font.lab=2,cex.lab=1.2,las=1,col="#36903E")
points(x=1:300,y=hypervolume.opt.m[,2],type="l",col="#43A245",lwd=2.5)
points(x=1:300,y=hypervolume.opt.m[,3],type="l",col="#61B05D",lwd=2.5)
points(x=1:300,y=hypervolume.opt.m[,4],type="l",col="#87C47F",lwd=2.5)
points(x=1:300,y=hypervolume.opt.m[,5],type="l",col="#A7D59E",lwd=2.5)
lines(x=1:300,y=rep(hypervolume.prop,300),col="#90365b",lwd=2.5,lty=3)

axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.03)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.03)
box(lwd=3)
legend(x="bottomright",inset=.03,legend=c("seed1","seed2","seed3","seed4","seed5","training"),
       lty = c(1,1,1,1,1,3), col=c("#36903E","#43A245", "#61B05D","#87C47F","#A7D59E","#90365b"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(0,175,187,50,max=255),rgb(205,16,118,180,max=255)))
par(mfrow=c(1,1))


pareto.p.all <- rbind(pareto.p.collection[[1]],pareto.p.collection[[2]],pareto.p.collection[[3]],
                      pareto.p.collection[[4]],pareto.p.collection[[5]])
pareto.p.all <- pareto.p.all[!duplicated(pareto.p.all[,1:8]),]
rownames(pareto.p.all) <- NULL
pareto.p.all$uncertainty <- 4*pareto.p.all$hys.sd*pareto.p.all$enth.sd
pareto.p.all <- pareto.p.all[order(pareto.p.all$uncertainty,decreasing = T),]

# pareto.p.all.num <-fn.paretoFront(f1=pareto.p.all$hys.LCB,f2=pareto.p.all$enth.LCB,ID=c(1:nrow(pareto.p.all)))
# new.pareto.p.all <- pareto.p.all[pareto.p.all.num,]
# new.pareto.p.all$uncertainty <- 4*new.pareto.p.all$hys.sd*new.pareto.p.all$enth.sd
# new.pareto.p.all <- new.pareto.p.all[order(new.pareto.p.all$uncertainty,decreasing = T),]


pareto.collection[[1]] <- optimization[[1]][[300]][order(optimization[[1]][[300]]$hys.mean,decreasing=T),]
pareto.collection[[2]] <- optimization[[2]][[300]][order(optimization[[2]][[300]]$hys.mean,decreasing=T),]
pareto.collection[[3]] <- optimization[[3]][[300]][order(optimization[[3]][[300]]$hys.mean,decreasing=T),]
pareto.collection[[4]] <- optimization[[4]][[300]][order(optimization[[4]][[300]]$hys.mean,decreasing=T),]
pareto.collection[[5]] <- optimization[[5]][[300]][order(optimization[[5]][[300]]$hys.mean,decreasing=T),]


## all pareto in mean space


plot(x=pareto.m.collection[[1]]$hys.mean,y=pareto.m.collection[[1]]$enth.mean,
     xlim=c(0,60),ylim=c(-40,0),col="#1C65A3",type="S",lwd=2,main="mean space",
     xlab="Hysetresis mean",ylab="Enthalpy mean",tcl=0.4,cex.axis=1.2,font=2,cex.lab=1.2,font.lab=2,las=1)
points(x=pareto.m.collection[[1]]$hys.mean,y=pareto.m.collection[[1]]$enth.mean,
       col="#1C65A3",pch=22,bg=rgb(28,101,163,100,max=255),cex=1.5,lwd=1.5)

lines(x=pareto.m.collection[[2]]$hys.mean,y=pareto.m.collection[[2]]$enth.mean,
      col="#1C6FAE",type="S",lwd=2)
points(x=pareto.m.collection[[2]]$hys.mean,y=pareto.m.collection[[2]]$enth.mean,
       col="#1C6FAE",pch=22,bg=rgb(28,111,174,100,max=255),cex=1.5)

lines(x=pareto.m.collection[[3]]$hys.mean,y=pareto.m.collection[[3]]$enth.mean,
      col="#4993C0",type="S",lwd=2)
points(x=pareto.m.collection[[3]]$hys.mean,y=pareto.m.collection[[3]]$enth.mean,
       col="#4993C0",pch=22,bg=rgb(73,147,192,100,max=255),cex=1.5)

lines(x=pareto.m.collection[[4]]$hys.mean,y=pareto.m.collection[[4]]$enth.mean,
      col="#64AAD2",type="S",lwd=2)
points(x=pareto.m.collection[[4]]$hys.mean,y=pareto.m.collection[[4]]$enth.mean,
       col="#64AAD2",pch=22,bg=rgb(100,170,210,100,max=255),cex=1.5)
lines(x=pareto.m.collection[[5]]$hys.mean,y=pareto.m.collection[[5]]$enth.mean,
      col="#93CDDF",type="S",lwd=2)
points(x=pareto.m.collection[[5]]$hys.mean,y=pareto.m.collection[[5]]$enth.mean,
       col="#93CDDF",pch=22,bg=rgb(147,205,223,100,max=255),cex=1.5)
points(x=pareto.m.all[1:4,]$hys.mean,y=pareto.m.all[1:4,]$enth.mean,
       col="red",pch=24,bg=rgb(255,0,0,100,max=255),cex=2,lwd=2)


lines(x=property.pareto$hysteresis,y=property.pareto$enthalpy,
      col="#90365b",type="S",lwd=2)
points(x=property.pareto$hysteresis,y=property.pareto$enthalpy,
       col="#90365b",pch=23,bg=rgb(144,54,91,100,max=255),cex=1.5)

# lines(x=pareto.collection[[1]]$hys.mean,y=pareto.collection[[1]]$enth.mean,
#       col="#248a8a",type="S",lwd=1)

points(x=pareto.collection[[1]]$hys.mean,y=pareto.collection[[1]]$enth.mean,
       col="#248a8a",pch=21,bg=rgb(36,138,138,100,max=255),cex=1.5,lwd=1.5)

# lines(x=pareto.collection[[2]]$hys.mean,y=pareto.collection[[3]]$enth.mean,
#       col="#16bfbf",type="S",lwd=1)

points(x=pareto.collection[[2]]$hys.mean,y=pareto.collection[[2]]$enth.mean,
       col="#16bfbf",pch=21,bg=rgb(22,191,191,100,max=255),cex=1.5,lwd=1.5)

# lines(x=pareto.collection[[3]]$hys.mean,y=pareto.collection[[3]]$enth.mean,
#       col="#a0e5e3",type="S",lwd=1)
points(x=pareto.collection[[3]]$hys.mean,y=pareto.collection[[3]]$enth.mean,
       col="#a0e5e3",pch=21,bg=rgb(160,229,227,100,max=255),cex=1.5,lwd=1.5)

# lines(x=pareto.collection[[4]]$hys.mean,y=pareto.collection[[4]]$enth.mean,
#       col="#d2f2ed",type="S",lwd=1)
points(x=pareto.collection[[4]]$hys.mean,y=pareto.collection[[4]]$enth.mean,
       col="#d2f2ed",pch=21,bg=rgb(210,242,237,100,max=255),cex=1.5,lwd=1.5)
# lines(x=pareto.collection[[5]]$hys.mean,y=pareto.collection[[5]]$enth.mean,
#       col="#238080",type="S",lwd=1)
points(x=pareto.collection[[5]]$hys.mean,y=pareto.collection[[5]]$enth.mean,
       col="#238080",pch=21,bg=rgb(35,128,128,100,max=255),cex=1.5,lwd=1.5)
points(x=pareto.all[1:4,]$hys.mean,y=pareto.all[1:4,]$enth.mean,
       col="purple",pch=24,bg=rgb(128,0,128,100,max=255),cex=2,lwd=2)


axis(side=1,lwd.ticks =3,tick=T,labels = F,tck=0.02)
axis(side=2,lwd.ticks=3,tick=T,labels = F,tck=0.02)
box(lwd=3)
legend(x="topright",inset=.03,legend=c("NSGA2-LCB","NSGA-mean","rawdata","LCB selected","mean selected"),
       lty = c(NA,1,1,NA,NA),pch =c(22,22,23,24,24), col=c("#248a8a","#1C65A3", "#90365b","purple","red"),lwd = 2,bty="n",text.font = 2,
       pt.bg=c(rgb(22,191,191,100,max=255),rgb(28,101,163,100,max=255),rgb(144,54,91,100,max=255),
               rgb(128,0,128,100,max=255),rgb(255,0,0,100,max=255)))



