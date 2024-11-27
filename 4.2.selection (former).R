

cand.mean.dist <- fn.cand.dist(pareto = vir.mean.pareto,
                               f1 = vir.mean.pareto$hysteresis,
                               f2 = vir.mean.pareto$enthalpy,
                               target = c(0,-60))

data.distance <- fn.distance(f1 = rawdata$hysteresis, f2 = rawdata$enthalpy, target = c(0,-60))
cand.mean.disego <- fn.cand.disego(pareto = vir.mean.pareto,
                                   f1 = vir.mean.pareto$hysteresis,
                                   f2 = vir.mean.pareto$enthalpy,
                                   sd1 = vir.mean.pareto$hys.sd,
                                   sd2 = vir.mean.pareto$enth.sd,
                                   target = c(0,-60),
                                   property = data.distance)
cand.mean.unc <- fn.cand.uncertainty(pareto = vir.mean.pareto,
                                     sd1 = vir.mean.pareto$hys.sd,
                                     sd2 = vir.mean.pareto$enth.sd)


cand.LCB.unc <- fn.cand.uncertainty(pareto = vir.LCB.pareto,
                                    sd1 = vir.LCB.pareto$hys.sd,
                                    sd2 = vir.LCB.pareto$enth.sd)

cand.EI.unc <- fn.cand.uncertainty(pareto = vir.EI.pareto,
                                   sd1 = vir.EI.pareto$hys.sd,
                                   sd2 = vir.EI.pareto$enth.sd)


cand.EI.dist <- fn.cand.dist(pareto = vir.EI.pareto,
                             f1 = vir.EI.pareto$hysteresis,
                             f2 = vir.EI.pareto$enthalpy,
                             target = c(0,-60))

cand.EI.disego <- fn.cand.disego(pareto = vir.EI.pareto,
                                 f1 = vir.EI.pareto$hysteresis,
                                 f2 = vir.EI.pareto$enthalpy,
                                 sd1 = vir.EI.pareto$hys.sd,
                                 sd2 = vir.EI.pareto$enth.sd,
                                 target = c(0,-60),
                                 property = data.distance)


cand.LCB.dist <- fn.cand.dist(pareto = vir.LCB.pareto,
                              f1 = vir.LCB.pareto$hysteresis,
                              f2 = vir.LCB.pareto$enthalpy,
                              target = c(0,-60))

cand.LCB.disego <- fn.cand.disego(pareto = vir.LCB.pareto,
                                  f1 = vir.LCB.pareto$hysteresis,
                                  f2 = vir.LCB.pareto$enthalpy,
                                  sd1 = vir.LCB.pareto$hys.sd,
                                  sd2 = vir.LCB.pareto$enth.sd,
                                  target = c(0,-60),
                                  property = data.distance)




# min(vir.ID$enth.mean)
# min(rawdata$enthalpy)

candidate1 <- data.frame(method = c("mean.dist","mean.disego","mean.unc",
                                    "EI.dist","EI.disego","EI.unc",
                                    "LCB.dist","LCB.disego","LCB.unc"),
                         rbind(cand.mean.dist[,-12],cand.mean.disego[,-12],cand.mean.unc[,-12],
                               cand.EI.dist[,-12],cand.EI.disego[,-12],cand.EI.unc[,-12],
                               cand.LCB.dist[,-12],cand.LCB.disego[,-12],cand.LCB.unc[,-12]))


plot(x=rawdata$hysteresis,y=rawdata$enthalpy,cex=1,pch=0,col="darkgreen",xlim = c(0,120),ylim = c(-35,-2))
points(x=vir.ID$hys.mean,y=vir.ID$enth.mean,cex=1,pch=1,col="black")
points(x=vir.ID$hys.mean[vir.mean.pareto.num],y=vir.ID$enth.mean[vir.mean.pareto.num],cex=2,pch=16,col="red")
points(x=rawdata$hysteresis[property.pareto.num],y=rawdata$enthalpy[property.pareto.num],cex=1.5,pch=16,col="blue")
points(x=candidate1$hysteresis[9],y=candidate1$enthalpy[9],cex=2.5,pch=11,col="orange")



randf.hys <- randomForest(formula=formula.f1, data=data.hys, ntree=500)
randf.hres <- predict(randf.hys,data.hys[,1:4])