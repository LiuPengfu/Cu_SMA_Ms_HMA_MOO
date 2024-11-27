# ## Feature calculation
# calFeature <- function(dataReal,features,elements,selfeature,H){
#   
#   # dataElement <- dataReal[,1:6] / 100 # atom fraction
#   dataElement <- dataReal[,elements] / 100# atom fraction
#   # dataElement <- dataElement / 100
#   
#   if(selfeature == "S.mix"){
#     dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
#     dataTest <- dataElement * log(dataElement)
#     dataTest[is.na(dataTest)] <- 0
#     dataReal$S.mix <- -8.314*rowSums(dataTest)
#   }
#   else if(selfeature == "r.atom.mix"){
#     property <- matrix(features[colnames(dataElement),"r.atom"])
#     dataReal$r.atom.mix <- as.numeric(as.matrix(dataElement) %*% property)
#   }
#   else if(selfeature == "delta.r.mix"){
#     dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
#     for(i in 1:ncol(dataElement)){
#       dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"r.atom"]/dataReal$r.atom.mix)^2
#     }
#     dataReal$delta.r.mix <- (rowSums(dataTest))^0.5
#     
#     
#   }
#   else if(selfeature == "mismatch.x.mix"){
#     dataTest <- matrix(NA,nrow(dataReal),ncol(dataElement)*(ncol(dataElement)-1)/2)
#     u <- 1
#     for(i in 1:(ncol(dataElement)-1)){
#       for(j in (i+1):ncol(dataElement)){
#         dataTest[,u] <- dataElement[,i]*dataElement[,j]*abs(features[which(rownames(features)==colnames(dataElement)[i]),"x"]-features[which(rownames(features)==colnames(dataElement)[j]),"x"])
#         u <- u+1
#       }
#     }
#     dataReal$mismatch.x.mix <- rowSums(dataTest)
#   }
#   else if(selfeature == "lamda.mix"){
#     dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
#     for(i in 1:nrow(dataReal)){
#       dataTest[i,] <- (1-(((dataReal$r.atom.mix[i]+min(dataElement[i,]))^2-
#                              dataReal$r.atom.mix[i]^2)/(dataReal$r.atom.mix[i]+min(dataElement[i,]))^2)^0.5)/
#         (1-(((dataReal$r.atom.mix[i]+max(dataElement[i,]))^2-dataReal$r.atom.mix[i]^2)
#             /(dataReal$r.atom.mix[i]+max(dataElement[i,]))^2)^0.5)
#     }
#     dataReal$lamda.mix <- dataTest[,1]
#   }
#   else if(selfeature == "Tm.mix"){
#     property <- matrix(features[colnames(dataElement),"Tm"])
#     dataReal$Tm.mix <- as.numeric(as.matrix(dataElement) %*% property)
#   }
#   else if(selfeature == "ou.mix"){
#     dataReal$ou.mix <- dataReal$Tm.mix*dataReal$S.mix/abs(dataReal$H.mix*1000)
#   }
#   else if(selfeature == "delta.x.mix"){
#     dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
#     for(i in 1:ncol(dataElement)){
#       dataTest[,i] <- (features[which(rownames(features)==colnames(dataElement)[i]),"x"]-dataReal$x.mix)^2*dataElement[,i]
#     }
#     dataReal$delta.x.mix <- (rowSums(dataTest))^0.5
#   }
#   else if(selfeature == "H.mix"){
#     dataTest <- matrix(NA,nrow(dataReal),ncol(dataElement)*(ncol(dataElement)-1)/2)
#     u=1
#     for(i in 1:(ncol(dataElement)-1)){
#       for(j in (i+1):ncol(dataElement))
#       {
#         {
#           m <- which(rownames(H) == colnames(dataElement)[i])
#           n <- which(colnames(H) == colnames(dataElement)[j])
#           dataTest[,u] <- 4*dataElement[,i]*dataElement[,j]*as.numeric(as.character(H[m,n]))
#           u <- u+1
#         }
#       }
#       dataReal$H.mix <- rowSums(dataTest)
#     }
#   }
#   else if(selfeature == "x.mix"){
#     property <- matrix(features[colnames(dataElement),"x"])
#     dataReal$x.mix <- as.numeric(as.matrix(dataElement) %*% property)
#     
#   }
#   else if(selfeature == "VEC.mix"){
#     property <- matrix(features[colnames(dataElement),"VEN"])
#     dataReal$VEC.mix <- as.numeric(as.matrix(dataElement) %*% property)
#     
#   }
#   else if(selfeature == "ea.mix"){
#     #????????
#     #caonima <- matrix(features[colnames(dataElement),"Tm"] / features[colnames(dataElement),"anum"]) 
#     #dataReal$ea.mix <- as.matrix(dataElement) %*% caonima
#     dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
#     for(i in 1:ncol(dataElement)){
#       dataTest[,i] <- features[which(rownames(features)==colnames(dataElement)[i]),"VEN"]/features[which(rownames(features)==colnames(dataElement)[i]),"anum"]*dataElement[,i]
#     }
#     dataReal$ea.mix <- rowSums(dataTest)
#   }
#   else if(selfeature == "energy.cohesive.mix"){
#     property <- matrix(features[colnames(dataElement),"energy.cohesive"])
#     dataReal$energy.cohesive.mix <- as.numeric(as.matrix(dataElement) %*% property)
#     # for(i in 1:ncol(dataElement)){
#     #   dataTest[,i] = features[which(rownames(features)==colnames(dataElement)[i]),"energy.cohesive"]*dataElement[,i]
#     # }
#     # dataReal$energy.cohesive.mix = rowSums(dataTest)
#   }
#   else if(selfeature == "YM.mix"){
#     property <- matrix(features[colnames(dataElement),"YM"])
#     dataReal$YM.mix <- as.numeric(as.matrix(dataElement) %*% property)
#     
#   }
#   else if(selfeature == "Poisson.Ratio.mix"){
#     property <- matrix(features[colnames(dataElement),"Poisson.Ratio"])
#     dataReal$Poisson.Ratio.mix <- as.numeric(as.matrix(dataElement) %*% property)
#   }
#   else if(selfeature == "cs.mix"){ 
#     property <- matrix(features[colnames(dataElement),"cs"])
#     dataReal$cs.mix <- as.numeric(as.matrix(dataElement) %*% property)
#   }
#   else if(selfeature == "dor.mix"){
#     property <- matrix(features[colnames(dataElement),"dor"])
#     dataReal$dor.mix <- as.numeric(as.matrix(dataElement) %*% property)
#   }
#   
#   return(dataReal)
# }


fn.calFeature <- function(dataReal,features,elements,H){
  
  # if(sum(rowSums(dataReal[,elements]==100))==nrow(dataReal))
  
  dataElement <- dataReal[,elements] / 100# atom fraction
  
  ## S.mix
  dataTest <- dataElement * log(dataElement)
  dataTest[is.na(dataTest)] <- 0
  dataReal$S.mix <- -8.314*rowSums(dataTest)
  
  ## r.atom.mix
  property <- matrix(features[colnames(dataElement),"r.atom"])
  dataReal$r.atom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.r.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"r.atom"]/dataReal$r.atom.mix)^2
  }
  dataReal$delta.r.mix <- (rowSums(dataTest))^0.5
  
  ## mismatch.x.mix
  dataTest <- matrix(NA,nrow(dataReal),ncol(dataElement)*(ncol(dataElement)-1)/2)
  u <- 1
  for(i in 1:(ncol(dataElement)-1)){
    for(j in (i+1):ncol(dataElement)){
      dataTest[,u] <- dataElement[,i]*dataElement[,j]*abs(features[which(rownames(features)==colnames(dataElement)[i]),"x"]-features[which(rownames(features)==colnames(dataElement)[j]),"x"])
      u <- u+1
    }
  }
  dataReal$mismatch.x.mix <- rowSums(dataTest)
  
  ## lamda.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:nrow(dataReal)){
    dataTest[i,] <- (1-(((dataReal$r.atom.mix[i]+min(dataElement[i,]))^2-
                           dataReal$r.atom.mix[i]^2)/(dataReal$r.atom.mix[i]+min(dataElement[i,]))^2)^0.5)/
      (1-(((dataReal$r.atom.mix[i]+max(dataElement[i,]))^2-dataReal$r.atom.mix[i]^2)
          /(dataReal$r.atom.mix[i]+max(dataElement[i,]))^2)^0.5)
  }
  dataReal$lamda.mix <- dataTest[,1]
  
  ## Tm.mix
  property <- matrix(features[colnames(dataElement),"Tm"])
  dataReal$Tm.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## x.mix
  property <- matrix(features[colnames(dataElement),"x"])
  dataReal$x.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.x.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- (features[which(rownames(features)==colnames(dataElement)[i]),"x"]-dataReal$x.mix)^2*dataElement[,i]
  }
  dataReal$delta.x.mix <- (rowSums(dataTest))^0.5
  
  ## H.mix
  dataTest <- matrix(NA,nrow(dataReal),ncol(dataElement)*(ncol(dataElement)-1)/2)
  u=1
  for(i in 1:(ncol(dataElement)-1)){
    for(j in (i+1):ncol(dataElement))
    {
      m <- which(rownames(H) == colnames(dataElement)[i])
      n <- which(colnames(H) == colnames(dataElement)[j])
      dataTest[,u] <- 4*dataElement[,i]*dataElement[,j]*as.numeric(as.character(H[m,n]))
      u <- u+1
    }
    dataReal$H.mix <- rowSums(dataTest)
  }
  
  ## ou.mix
  dataReal$ou.mix <- dataReal$Tm.mix*dataReal$S.mix/abs(dataReal$H.mix*1000)
  
  ## ven.mix
  property <- matrix(features[colnames(dataElement),"ven"])
  dataReal$ven.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## ea.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- features[which(rownames(features)==colnames(dataElement)[i]),"ven"]/features[which(rownames(features)==colnames(dataElement)[i]),"anum"]*dataElement[,i]
  }
  dataReal$ea.mix <- rowSums(dataTest)
  
  ## energy.cohesive.mix
  property <- matrix(features[colnames(dataElement),"energy.cohesive"])
  dataReal$energy.cohesive.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## ym.mix
  property <- matrix(features[colnames(dataElement),"ym"])
  dataReal$ym.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.ym.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"ym"]/dataReal$ym.mix)^2
  }
  dataReal$delta.ym.mix <- (rowSums(dataTest))^0.5
  
  
  ## Poisson.Ratio.mix
  property <- matrix(features[colnames(dataElement),"Poisson.Ratio"])
  dataReal$Poisson.Ratio.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.Poisson.Ratio.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"Poisson.Ratio"]/dataReal$Poisson.Ratio.mix)^2
  }
  dataReal$delta.Poisson.Ratio.mix <- (rowSums(dataTest))^0.5
  
  ## cs.mix
  property <- matrix(features[colnames(dataElement),"cs"])
  dataReal$cs.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.cs.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"cs"]/dataReal$cs.mix)^2
  }
  dataReal$delta.cs.mix <- (rowSums(dataTest))^0.5
  
  ## rm.mix
  property <- matrix(features[colnames(dataElement),"rm"])
  dataReal$rm.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.rm.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"rm"]/dataReal$rm.mix)^2
  }
  dataReal$delta.rm.mix <- (rowSums(dataTest))^0.5
  
  ## bm.mix
  property <- matrix(features[colnames(dataElement),"bm"])
  dataReal$bm.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.bm.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"bm"]/dataReal$bm.mix)^2
  }
  dataReal$delta.bm.mix <- (rowSums(dataTest))^0.5
  
  ## Efus.mix
  property <- matrix(features[colnames(dataElement),"Efus"])
  dataReal$Efus.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.Efus.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"Efus"]/dataReal$Efus.mix)^2
  }
  dataReal$delta.Efus.mix <- (rowSums(dataTest))^0.5
  
  ## s.solid.mix
  property <- matrix(features[colnames(dataElement),"s.solid"])
  dataReal$s.solid.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.S.solid.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"s.solid"]/dataReal$s.solid.mix)^2
  }
  dataReal$delta.s.solid.mix <- (rowSums(dataTest))^0.5
  
  ## Debye.mix
  property <- matrix(features[colnames(dataElement),"Debye"])
  dataReal$Debye.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.Debye.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"Debye"]/dataReal$Debye.mix)^2
  }
  dataReal$delta.Debye.mix <- (rowSums(dataTest))^0.5
  #
  ## enth.vap.mix
  property <- matrix(features[colnames(dataElement),"enth.vap"])
  dataReal$enth.vap.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.enth.vap.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"enth.vap"]/dataReal$enth.vap.mix)^2
  }
  dataReal$delta.enth.vap.mix <- (rowSums(dataTest))^0.5
  
  ## enth.atom.mix
  property <- matrix(features[colnames(dataElement),"enth.atom"])
  dataReal$enth.atom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.enth.atom.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"enth.atom"]/dataReal$enth.atom.mix)^2
  }
  dataReal$delta.enth.atom.mix <- (rowSums(dataTest))^0.5
  
  ## wf.mix
  property <- matrix(features[colnames(dataElement),"wf"])
  dataReal$wf.mix <- as.numeric(as.matrix(dataElement) %*% property)

  ## delta.wf.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"wf"]/dataReal$wf.mix)^2
  }
  dataReal$delta.wf.mix <- (rowSums(dataTest))^0.5

  ## amass.mix
  property <- matrix(features[colnames(dataElement),"amass"])
  dataReal$amass.mix <- as.numeric(as.matrix(dataElement) %*% property)

  ## delta.amass.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"amass"]/dataReal$amass.mix)^2
  }
  dataReal$delta.amass.mix <- (rowSums(dataTest))^0.5

  ## conT.mix
  property <- matrix(features[colnames(dataElement),"conT"])
  dataReal$conT.mix <- as.numeric(as.matrix(dataElement) %*% property)

  ## delta.conT.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"conT"]/dataReal$conT.mix)^2
  }
  dataReal$delta.conT.mix <- (rowSums(dataTest))^0.5

  
  ## cm.mix
  property <- matrix(features[colnames(dataElement),"cm"])
  dataReal$cm.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.cm.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"cm"]/dataReal$cm.mix)^2
  }
  dataReal$delta.cm.mix <- (rowSums(dataTest))^0.5
  
  ## cp.mix
  property <- matrix(features[colnames(dataElement),"cp"])
  dataReal$cp.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.cp.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"cp"]/dataReal$cp.mix)^2
  }
  dataReal$delta.cp.mix <- (rowSums(dataTest))^0.5
  
  ## m.hcap.mix
  property <- matrix(features[colnames(dataElement),"m.hcap"])
  dataReal$m.hcap.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.m.hcap.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"m.hcap"]/dataReal$m.hcap.mix)^2
  }
  dataReal$delta.m.hcap.mix <- (rowSums(dataTest))^0.5
  
  ## cne.mix
  property <- matrix(features[colnames(dataElement),"cne"])
  dataReal$cne.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.cne.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"cne"]/dataReal$cne.mix)^2
  }
  dataReal$delta.cne.mix <- (rowSums(dataTest))^0.5
  
  ## hmo.mix
  property <- matrix(features[colnames(dataElement),"hmo"])
  dataReal$hmo.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.hmo.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"hmo"]/dataReal$hmo.mix)^2
  }
  dataReal$delta.hmo.mix <- (rowSums(dataTest))^0.5
  
  ## hom.mix
  property <- matrix(features[colnames(dataElement),"hom"])
  dataReal$hom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.hom.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"hom"]/dataReal$hom.mix)^2
  }
  dataReal$delta.hom.mix <- (rowSums(dataTest))^0.5
  
  ## enth.vac.mix
  property <- matrix(features[colnames(dataElement),"enth.vac"])
  dataReal$enth.vac.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.enth.vac.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"enth.vac"]/dataReal$enth.vac.mix)^2
  }
  dataReal$delta.enth.vac.mix <- (rowSums(dataTest))^0.5
  
  ## mcs.mix
  property <- matrix(features[colnames(dataElement),"mcs"])
  dataReal$mcs.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.mcs.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"mcs"]/dataReal$mcs.mix)^2
  }
  dataReal$delta.mcs.mix <- (rowSums(dataTest))^0.5
  
  ## vp.mix
  property <- matrix(features[colnames(dataElement),"vp"])
  dataReal$vp.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.vp.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"vp"]/dataReal$vp.mix)^2
  }
  dataReal$delta.vp.mix <- (rowSums(dataTest))^0.5
  
  ## vatom.mix
  property <- matrix(features[colnames(dataElement),"vatom"])
  dataReal$vatom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.vatom.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"vatom"]/dataReal$vatom.mix)^2
  }
  dataReal$delta.vatom.mix <- (rowSums(dataTest))^0.5
  
  return(dataReal)
}



fn.vir.calFeature <- function(dataReal,features,elements,H){
  
  # if(sum(rowSums(dataReal[,elements]==100))==nrow(dataReal))
  
  dataElement <- dataReal[,elements] / 100# atom fraction
  
  ## S.mix
  dataTest <- dataElement * log(dataElement)
  dataTest[is.na(dataTest)] <- 0
  dataReal$S.mix <- -8.314*rowSums(dataTest)
  
  ## r.atom.mix
  property <- matrix(features[colnames(dataElement),"r.atom"])
  dataReal$r.atom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.r.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"r.atom"]/dataReal$r.atom.mix)^2
  }
  dataReal$delta.r.mix <- (rowSums(dataTest))^0.5
  
  ## x.mix
  property <- matrix(features[colnames(dataElement),"x"])
  dataReal$x.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.x.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- (features[which(rownames(features)==colnames(dataElement)[i]),"x"]-dataReal$x.mix)^2*dataElement[,i]
  }
  dataReal$delta.x.mix <- (rowSums(dataTest))^0.5
  
  ## Poisson.Ratio.mix
  property <- matrix(features[colnames(dataElement),"Poisson.Ratio"])
  dataReal$Poisson.Ratio.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.Poisson.Ratio.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"Poisson.Ratio"]/dataReal$Poisson.Ratio.mix)^2
  }
  dataReal$delta.Poisson.Ratio.mix <- (rowSums(dataTest))^0.5
  
  ## Efus.mix
  property <- matrix(features[colnames(dataElement),"Efus"])
  dataReal$Efus.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## wf.mix
  property <- matrix(features[colnames(dataElement),"wf"])
  dataReal$wf.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## amass.mix
  property <- matrix(features[colnames(dataElement),"amass"])
  dataReal$amass.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## cm.mix
  property <- matrix(features[colnames(dataElement),"cm"])
  dataReal$cm.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## hom.mix
  property <- matrix(features[colnames(dataElement),"hom"])
  dataReal$hom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.hom.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"hom"]/dataReal$hom.mix)^2
  }
  dataReal$delta.hom.mix <- (rowSums(dataTest))^0.5
  
  return(dataReal)
}


fn.hys.feature <- function(dataReal,features,elements,H){
  
  dataElement <- dataReal[,elements] / 100# atom fraction
  ## Efus.mix
  property <- matrix(features[colnames(dataElement),"Efus"])
  dataReal$Efus.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## enth.vap.mix
  property <- matrix(features[colnames(dataElement),"enth.vap"])
  dataReal$enth.vap.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  
  ## delta.vatom.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"vatom"]/dataReal$vatom.mix)^2
  }
  dataReal$delta.vatom.mix <- (rowSums(dataTest))^0.5
  return(dataReal)
  
}

fn.enth.feature <- function(){
  
  dataElement <- dataReal[,elements] / 100# atom fraction
  ## delta.vp.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"vp"]/dataReal$vp.mix)^2
  }
  dataReal$delta.vp.mix <- (rowSums(dataTest))^0.5
  
  ## hom.mix
  property <- matrix(features[colnames(dataElement),"hom"])
  dataReal$hom.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.cp.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"cp"]/dataReal$cp.mix)^2
  }
  dataReal$delta.cp.mix <- (rowSums(dataTest))^0.5
  
  ## Poisson.Ratio.mix
  property <- matrix(features[colnames(dataElement),"Poisson.Ratio"])
  dataReal$Poisson.Ratio.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  ## delta.bm.mix
  dataTest <- matrix(nrow=nrow(dataReal),ncol=ncol(dataElement))
  for(i in 1:ncol(dataElement)){
    dataTest[,i] <- dataElement[,i]*(1-features[which(rownames(features)==colnames(dataElement)[i]),"bm"]/dataReal$bm.mix)^2
  }
  dataReal$delta.bm.mix <- (rowSums(dataTest))^0.5
  
  ## m.hcap.mix
  property <- matrix(features[colnames(dataElement),"m.hcap"])
  dataReal$m.hcap.mix <- as.numeric(as.matrix(dataElement) %*% property)
  
  
  
  
  return(dataReal)
  
}


fn.replace <- function(dataReal,elements=data.elements){
  dataElement <- dataReal[,elements]/100
  ## replace
  data.max <- apply(dataElement,2,max)
  fn.divide <- function(x,y){return(x/y)}
  rel.rep <- t(apply(dataElement[,-c(1,2)], 1,fn.divide,data.max[-c(1,2)]))# remove Ti Ni
  dataReal$replace <- rowSums(rel.rep)
  return(dataReal)
}


## Z-Score
fn.z.score <- function(data){
  average = colMeans(data)
  sd = apply(as.matrix(data), 2, sd) #2 represent the method by col
  
  #generate a dataframe the same size of data
  average = data.frame(t(average))
  average = average[rep(seq_len(1), times = nrow(data)),]
  
  sd = data.frame(t(sd))
  sd = sd[rep(seq_len(1), times = nrow(data)), ]
  result = list(data.frame((data - average)/sd), ave=average[1,], sd=sd[1,])
  #names(result) = c("Z", "ave","sd")
  return(result)
}



fn.maxmin.scale <- function(data){
  # data <- data.frame(data)
    max <- t(apply(data,2,max))
    max <- data.frame(max)
    max <- max[rep(seq_len(1), times = nrow(data)),]
    
    min <- t(apply(data,2,min))
    min <- data.frame(min)
    min <- min[rep(seq_len(1), times = nrow(data)),]
    result <- (data-min)/abs(max-min)
   

  return(result)
}

fn.maxmin.scale.single <- function(data){

  max <- max(data)
  min <- min(data)
  result <- (data-min)/abs(max-min)
  return(result)
  
}



  
fn.vir.maxmin.scale <- function(vir,data){
  
  max <- t(apply(data,2,max))
  max <- data.frame(max)
  max <- max[rep(seq_len(1), times = nrow(vir)),]
  
  min <- t(apply(data,2,min))
  min <- data.frame(min)
  min <- min[rep(seq_len(1), times = nrow(vir)),]
  result <- (vir-min)/abs(max-min)
  return(result)
}


fn.relinf.gb <- function(data,formula,name,n,inf = 5){
  
  rel.inf <- data.frame(name)
  for (i in 1:n) {
    gb <- gbm(formula,distribution = "gaussian",n.cores = 5,
              data=data,n.trees = 1000,interaction.depth = 7,shrinkage = 0.01,cv.folds = 10)
    rel.inf.temp <- summary(gb)
    rel.inf <- cbind(rel.inf,rel.inf=rel.inf.temp[name,"rel.inf"])
  }
  
  # colnames(rel.inf) <- NULL
  
  rel.inf$mean <- rowMeans(rel.inf[,-1])
  rel.inf <- rel.inf[order(rel.inf$mean,decreasing=T),]
  gb.name <- c(rel.inf[which(rel.inf$mean > inf),1])
  return(list(rel.inf,gb.name))
  
}


fn.rmse <- function(prediction,measure){
  
  rmse <- sqrt(colSums((prediction-measure)^2)/nrow(prediction))
  
  mean <- mean(rmse)
  sd <- sqrt(sum((rmse-mean)^2)/length(rmse))
  
  return(list(rmse=rmse,rmse.mean=mean,rmse.sd=sd))
}


fn.R2=function(prediction,measure){
  
  # y=unlist(y)
  mean <- colMeans(prediction)
  S1 <- colSums((measure-prediction)^2)
  S2 <- colSums((prediction-mean)^2)
  R2=1-S1/S2
  
  mean <- mean(R2)
  sd <- sqrt(sum((R2-mean)^2)/length(R2))
  
  return(list(R2,mean,sd))
}

fn.R2.single=function(prediction,measure){
  
  # y prediction x measure
  mean.y <- mean(prediction)
  RSS <- sum((measure-prediction)^2)
  TSS <- sum((prediction-mean.y)^2)
  R2=1-RSS/TSS
  return(R2)
  
  # rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
  # tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
  # rsq <- 1 - rss/tss
  # regss <- sum((preds - mean(preds)) ^ 2) ## regression sum of squares
  # regss / tss
  
  
}

## best subset selection for gaussian process                                
fn.best.subset = function(data,virtual,formula=formula.f1,method){
  
  features <- data[,-ncol(data)]
  response <- as.data.frame(data[,ncol(data)])
  test.response <- as.data.frame(virtual[,ncol(virtual)])
  namelist <- colnames(features)
  result <- list()
  # i=1
  for (i in 1:length(namelist)){
    subset <- t(combn(x=namelist,m=i))
    rmse <- c()
    for(j in 1:nrow(subset)){
      # j=6
      input.features <- as.data.frame(features[,subset[j,]])
      colnames(input.features) <- subset[j,]
      input.response <- response
      test.features <- as.data.frame(virtual[,subset[j,]])
      colnames(test.features) <- subset[j,]
      input <- cbind(input.features,input.response)
      if(method=="gp"){
        prediction <- fn.gp(input.features,input.response,test.features)
        rmse[j] <- sqrt(sum((prediction$mean - test.response)^2)/nrow(test.features))
      }
      if(method=="svr.rbf"){
        input <- cbind(input.features,input.response)
        prediction <- fn.svr.rbf(input,test.features,formula = formula)
        rmse[j] <- sqrt(sum((prediction - test.response)^2)/nrow(test.features))
      }
      if(method=="rf"){
        input <- cbind(input.features,input.response)
        prediction <- fn.rf(input,test.features,formula = formula)
        rmse[j] <- sqrt(sum((prediction - test.response)^2)/nrow(test.features))
      }
      if(method=="gbm"){
        input <- cbind(input.features,input.response)
        prediction <- fn.gbm(input,test.features,formula = formula)
        rmse[j] <- sqrt(sum((prediction - test.response)^2)/nrow(test.features))
      }

    }
    result[[i]] <- data.frame(subset,rmse)
    
  }
  
  return(result)
  
}


## vector split
fn.vec.split=function(k,datasize){
  sp.list = list()
  # set.seed(seed)
  n = rep(1:k,each=ceiling(datasize/k))[1:datasize]
  temp = n
  x = 1:k
  dataseq = 1:datasize
  sp.list = lapply(x,function(x) dataseq[temp == x])
  return(sp.list)
  # return(n)
}


## k-fold cross-validation
CV=function(k,datasize){
  cvlist = list()
  # set.seed(seed)
  n = rep(1:k,ceiling(datasize/k))[1:datasize]
  temp = sample(n,datasize)
  x = 1:k
  dataseq = 1:datasize
  cvlist = lapply(x,function(x) dataseq[temp == x])
  return(cvlist)
}

# set.seed(1111)
# CV(k=10,20)


## cross-validation
fn.cv <- function(data,formula,k,method,
                  svr=c(1,4),rf=100,gbm=c(500,7,0.1),gp=c(1,0.1,2),xgb=c(3,30),
                  svrl=c(1,1),svrp=c(1,1)){
  # set.seed(123)
  cv.number <- CV(k,nrow(data))
  res <- matrix(ncol = 2)
  res.cv <- list()
  mse.temp <- 0
  mse.all <- NULL
  rmse.temp <- 0
  rmse.all <- NULL
  for (i in 1:k) {
    m = unlist(cv.number[i])
    if(method=="svr.rbf")
      res.cv[[i]] <- fn.svr.rbf(data[-m,],data,formula,svr)
    else if(method=="rf")
      res.cv[[i]] <- fn.rf(data[-m,],data,formula,rf)
    else if(method=="gp")
      res.cv[[i]] <- fn.gp(data[-m,-ncol(data)],data[-m,ncol(data)],data[,-ncol(data)],gp)
    # else if(method=="gbm")
    #   res.cv[[i]] <- fn.gbm(data[-m,],data,formula,gbm)
    else if(method=="xgb")
      res.cv[[i]] <- fn.xgboost(data[-m,],data,formula,xgb)
    else if(method=="lm")
      res.cv[[i]] <- fn.lm(data[-m,],data,formula)
    else if(method=="nnet")
      res.cv[[i]] <- fn.nnet(data[-m,],data,formula)
    else if(method=="svr.lin")
      res.cv[[i]] <- fn.svr.lin(data[-m,],data,formula,svrl)
    else if(method=="svr.poly")
      res.cv[[i]] <- fn.svr.poly(data[-m,],data,formula,svrp)
    mse.temp <- sum((res.cv[[i]][m,]-data[m,ncol(data)])^2)/length(m)
    rmse.temp <- sqrt(mse.temp)
    mse.all <- c(mse.all,mse.temp)
    rmse.all <- c(rmse.all,rmse.temp)
    temp <- as.matrix(cbind(unlist(res.cv[[i]])[m],m))
    #colnames(temp) <- NULL
    res <- rbind(res,temp)
    print(i)
  }
  # mean <- rowMeans(res.boots)
  # sd <- apply(res.boots,1,sd)
  # res.boots$mean <- mean
  # res.boots$sd <- sd
  # return(list(res.cv,cv.number))
  res <- res[-1,]
  colnames(res) <- c("prediction","ID")
  rownames(res) <- NULL
  
  res <- data.frame(res[order(res[,2]),])
  res.cv <- data.frame(do.call("cbind",res.cv))
  res.cv$mean <- rowMeans(res.cv)
  res.cv$sd <- apply(res.cv[,1:k],1,sd)
  
  # mse <- mse/k
  mse <- mean(mse.all)
  # mse.sd <- sd(mse.all)
  # rmse <- sqrt(sum((res[,1]-data[,ncol(data)])^2)/nrow(data))
  # rmse <- sqrt(mse)
  rmse <- mean(rmse.all)
  mae <- sum(abs(res[,1]-data[,ncol(data)]))/nrow(data)
  
  return(list(res=res,rmse=rmse,mae=mae,cv=res.cv,cv.num = cv.number,
              mse=mse,mse.all=mse.all,rmse.all=rmse.all))
  #return(temp)
  #return(cv.number)
}

# fn.R2(cv.hys.gbm[,1:10],data.hys$hysteresis)
# 
# test <- fn.boots.cv(data.hys,formula.f1,k=5,boots=5,"gbm",gbm=c(200,5,0.1))
# fn.R2(test[,1:5],data.hys[,ncol(data.hys)])

fn.boots.cv <- function(data,formula,boots,k,method,svr=c(1,4),rf=100,gbm=c(500,7,0.1),gp=c(1,0.1,2),xgb=c(3,30)){
  set.seed(123)
  # k=10
  # cv.number <- CV(k,nrow(data))
  # res <- matrix(ncol = 2)
  
  res.all <- NA
  n <- nrow(data)
  
  for (j in 1:boots) {
    set.seed(10000+j)
    res <- NA
    res.cv <- list()
    cv.number <- CV(k,nrow(data))
    # if(j==1){
    #   samp <- sample(n,n,replace = F)
    # }else{
    #   samp <- sample(n,n,replace = T)
    # }
    samp <- sample(n,n,replace = F)
    for (i in 1:k) {
      # data.new <- data[samp,]
      data.new <- data
      rownames(data.new) <- NULL
      m = unlist(cv.number[i])
      if(method=="svr.rbf")
        res.cv[[i]] <- fn.svr.rbf(data.new[-m,],data,formula,svr)
      else if(method=="rf")
        res.cv[[i]] <- fn.rf(data.new[-m,],data,formula,rf)
      else if(method=="gp")
        res.cv[[i]] <- fn.gp(data.new[-m,-ncol(data.new)],data.new[-m,ncol(data)],data[,-ncol(data)],gp)
      else if(method=="gbm")
        res.cv[[i]] <- fn.gbm(data.new[-m,],data,formula,gbm)
      else if(method=="xgb")
        res.cv[[i]] <- fn.xgboost(data.new[-m,],data,formula,xgb)
      # temp <- as.matrix(cbind(prediction=unlist(res.cv[[i]])[m],ID=m))
      temp <- as.matrix(cbind(unlist(res.cv[[i]])[m],m))
      #colnames(temp) <- NULL
      res <- rbind(res,temp)
      # print(data.new[m,])
      # print(data[m,])
      print(i)
    }
    
    res <- res[-1,]
    colnames(res) <- c("prediction","ID")
    rownames(res) <- NULL
    res <- data.frame(res[order(res[,2]),])
    res.all <- cbind(res.all,res[,1]) # prediction results
    print(paste("Bootstrap",j,"finished."))
  }
  
  res.all <- data.frame(res.all[,-1])
  mean <- rowMeans(res.all)
  sd <- sqrt(rowSums((res.all-mean)^2)/boots)
  rmse <- sqrt(rowSums((res.all-data[,ncol(data)])^2)/boots)
  # 
  # 
  return(cbind(res.all,mean,sd))
  # return(res)
}

# k=10
# boots=100
# set.seed(11111)
# cv.number <- CV(10,nrow(data.hys))
# test <- fn.boots.cv(data.hys,formula.f1,boots=5,k=10,"svr.rbf",svr=c(best.hys.svr.cv[1,1:2]))



fn.cv.loop <- function(data,formula,k,method,loop,svr=c(1,4),rf=100,gbm=c(500,7,0.1),gp=c(1,0.1,2)){
  loop.result <- NA
  for(i in 1:loop)
  {
    set.seed(123456+i)
    cv.number <- CV(k,nrow(data))
    res <- matrix(ncol = 2)
    res.cv <- list()
    for (j in 1:k) {
      m = unlist(cv.number[j])
      if(method=="svr.rbf")
        res.cv[[j]] <- fn.svr.rbf(data[-m,],data,formula,svr)
      else if(method=="rf")
        res.cv[[j]] <- fn.rf(data[-m,],data,formula,rf)
      else if(method=="gp")
        res.cv[[j]] <- fn.gp(data[-m,-ncol(data)],data[-m,ncol(data)],data[,-ncol(data)],gp)
      else if(method=="gbm")
        res.cv[[j]] <- fn.gbm(data[-m,],data,formula,gbm)
      temp <- as.matrix(cbind(unlist(res.cv[[j]])[m],m))
      res <- rbind(res,temp)
      print(j)
    }
    print(paste("loop",i))
    
    res <- res[-1,]
    colnames(res) <- c("prediction","ID")
    rownames(res) <- NULL
    
    res <- data.frame(res[order(res[,2]),])
    loop.result <- cbind(loop.result,res[,1])
  }
  loop.result <- data.frame(loop.result[,-1])
  mean <- rowMeans(loop.result)
  sd <- sqrt(rowSums((loop.result-mean)^2)/loop)
  rmse <- sqrt(rowSums((loop.result-data[,ncol(data)])^2)/loop)
  # mae <- rowSums(abs(loop.result-data[,ncol(data)]))/loop
  # loop.result$rmse <- sqrt(sum((res[,1]-data[,ncol(data)])^2)/nrow(data))
  # loop.result$mae <- sum(abs(res[,1]-data[,ncol(data)]))/nrow(data)

  # return(cbind(loop.result,mean=mean,sd=sd))
  return(cbind(loop.result,mean=mean,sd=sd,rmse=rmse))
}



## boots validation
fn.boots <- function(data,formula,boots,method,svr=c(1,2),rf=300,gbm=c(400,3,0.01),gp=c(2,0.001,0.5)){
  
  n <- nrow(data)
  # res.boots <- matrix(nrow=nrow(data))
  res.boots <- NA
  # colnames(res.boots) <- NULL
  for (i in 1:boots) {
    set.seed(111111+i)
    samp <- sample(n,n,replace=T)
    if(method=="svr.rbf")
      res.boots <- cbind(res.boots,fn.svr.rbf(data[samp,],data[,-ncol(data)],formula,svr))
    else if(method=="rf")
      res.boots <- cbind(res.boots,fn.rf(data[samp,],data,formula,rf))
    else if(method=="gbm")
      res.boots <- cbind(res.boots,fn.gbm(data[samp,],data,formula,gbm))
    else if(method=="gp")
      res.boots <- cbind(res.boots,fn.gp(data[samp,-ncol(data)],data[samp,ncol(data)],data[,-ncol(data)],gp))
    print(i)
  }
  # colnames(res.boots) <- NULL
  res.boots <- as.data.frame(res.boots[,-1])
  
  mean <- rowMeans(res.boots)# mean predicted values
  sd <- sqrt(rowSums((res.boots-mean)^2)/boots)
  rmse <- sqrt(rowSums((res.boots-data[,ncol(data)])^2)/boots)
  # mae <- rowSums(abs(res.boots-data[,ncol(data)]))/boots
  
  res.boots$mean <- mean
  res.boots$sd <- sd
  res.boots$rmse <- rmse
  # res.boots$rmse <- rmse
  return(res.boots)
  # return(res.boots)
}





## Machine learning model ##

## svr.rbf 
fn.para.svr.rbf = function(data, formula){
  tuneResult.svr.rbf = tune(svm, formula, data = data,
                            kernel = "radial",
                            ranges = list(gamma = seq(1,5,1), cost = seq(1,5,1)))
  #ranges = list(gamma=10^(-3:1),cost=10^(-3:10))
  # print(tuneResult.svr.rbf)
  return(tuneResult.svr.rbf)
}


fn.model.svr.rbf = function(data.train, formula){
  parameter.svr.rbf = fn.para.svr.rbf(data.train, formula)
  gamma = parameter.svr.rbf$best.parameters[1]
  cost = parameter.svr.rbf$best.parameters[2]
  model = svm(formula, data = data.train, kernel = "radial", 
              gamma = gamma, cost = cost)
  return (model)
}

#??????????best performance
fn.svr.rbf=function(data.train,data.test,formula,para=c(1,2)){
  
  # parameter.svr.rbf = fn.para.svr.rbf(data.train,formula)
  # # print(parameter.svr.rbf)
  # gamma = parameter.svr.rbf$best.parameters[1]
  # cost = parameter.svr.rbf$best.parameters[2]
  # model = svm(formula, data = data.train, kernel = "radial",
  #             gamma = gamma, cost = cost)
  model = svm(formula, data = data.train, kernel = "radial",
              gamma = para[1], cost = para[2])
  
  predResult = predict(model,data.test)
  
  return(data.frame(predResult))
  
}


## gradient boosting
fn.gbm = function(data.train,data.test,formula,para=c(500,7,0.1)){
  
  set.seed(1234)
  model=gbm(formula,distribution = "gaussian",data.train,
            n.trees = para[1],interaction.depth = para[2],shrinkage = para[3],cv.folds = 5)
  predResult=predict(model,data.test)
  return(data.frame(predResult))
  
}

## random forest
fn.rf = function(data.train,data.test,formula,para=c(3,200)){
  
  #set.seed(seed)
  model = randomForest(formula, data=data.train,mtry=para[1],ntree=para[2])
  predResult = predict(model,data.test) 
  
  return(data.frame(predResult))
  
}

## gaussian process


fn.lm <- function(data.train,data.test,formula){
  
  model <- lm(formula = formula,data = data.train)
  result <- predict(model,data.test)
  return(data.frame(result))
}

fn.svr.lin=function(data.train,data.test,formula,para=c(1,2)){
  
  model = svm(formula, data = data.train, kernel = "linear",
              gamma = para[1], cost = para[2])
  
  predResult = predict(model,data.test)
  
  return(data.frame(predResult))
  
}

fn.svr.poly=function(data.train,data.test,formula,para=c(1,2)){
  
  model = svm(formula, data = data.train, kernel = "polynomial",
              gamma = para[1], cost = para[2])
  
  predResult = predict(model,data.test)
  
  return(data.frame(predResult))
  
}


fn.nnet=function(data.train,data.test,formula){
  
  model = nnet(formula, data=data.train,size=3,linout = T,trace = F)
  
  predResult = predict(model,data.test)
  
  return(data.frame(predResult))
  
}

fn.gp = function(training.data.x, training.data.y,virtual.data.x,para=c(5e-4,2,1e-3)){ 
  # model <- km(formula=~.,design=training.data.x, response=training.data.y,covtype = "exp",
  #             noise.var=rep(10^(1),nrow(training.data.x))) # better than lower noise.var
  # model <- km(formula=~.,design=training.data.x, response=training.data.y,covtype = "exp",
  #             noise.var=rep(var(training.data.y),nrow(training.data.x)))
  # model <- km(formula=~.,design=training.data.x, response=training.data.y,covtype = "exp",
  #             nugget = 1)
  
  model <- km(formula=~.,design=training.data.x, response=training.data.y,covtype = "powexp",
              lower = para[1], upper = para[2],nugget = para[3],control = list(trace=F))
  
  #Ms #lower = 5e-4, upper = 2,nugget = 1e-3
  #HMA #covtype = "powexp",lower = 1e-10, upper = 2,nugget = 1e-5
  
  prediction <- predict(model, virtual.data.x, "SK")
  # result <- data.frame(cbind(prediction$mean,prediction$sd))
  # colnames(result)=c("mean","sd")
  # return(data.frame(prediction$mean,prediction$sd))
  return(data.frame(prediction$mean))
}

# fn.gp(data.enth[,-ncol(data.enth)],data.enth[,ncol(data.enth)],virtual.enth[1:1000000,-1])

fn.xgboost <- function(data.train,data.test,formula= ~.,para=c(3,30),verbose=0){
  
  train_x = data.matrix(data.train[, -ncol(data.train)])
  train_y = data.train[,ncol(data.train)]
  
  test_x = data.matrix(data.test[, -ncol(data.test)])
  test_y = data.test[, ncol(data.test)]
  
  #define final training and testing sets
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  watchlist = list(train=xgb_train, test=xgb_test)
  
  #fit XGBoost model and display training and testing data at each round
  model = xgb.train(data = xgb_train, watchlist=watchlist,max.depth = para[1],nrounds = para[2],verbose=verbose)
  pred = predict(model,xgb_test)
  
  return(data.frame(pred))
  
}



fn.vir.xgboost <- function(data.train,data.test,formula= ~.,para=c(3,30)){
  
  train_x = data.matrix(data.train[, -ncol(data.train)])
  train_y = data.train[,ncol(data.train)]
  
  test_x = data.matrix(data.test[, -ncol(data.test)])
  test_y = data.test[, ncol(data.test)]
  
  #define final training and testing sets
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x)
  
  watchlist = list(train=xgb_train, test=xgb_test)
  
  #fit XGBoost model and display training and testing data at each round
  model = xgb.train(data = xgb_train, watchlist=watchlist,max.depth = para[1],nrounds = para[2],verbose=0)
  pred = predict(model,xgb_test)
  
  return(data.frame(pred))
  
}


fn.para.svr <- function(data,formula,param){
  result <- NA
  result1 <- NA
  i=1
  for (i in 1:nrow(param)) {
    result <- rbind(result,fn.boots(data,formula,boots=10,"svr.rbf",svr  = param[i,])[[3]])
    result1 <- rbind(result1,fn.cv(data,formula,k=10,"svr.rbf",svr  = param[i,])[[3]])
  }
  return(data.frame(para=param,boots=result[-1,],cv=result1[-1,]))
}

# svr.para <- expand.grid(gamma = c(1:20),cost=c(1:30))
# para.svr.hys <- fn.para.svr(data = data.gb.hys,formula = hysteresis~.,param = svr.para)
# para.svr.enth <- fn.para.svr(data = data.gb.enth,formula = enthalpy~.,param = svr.para)

## random forest
fn.para.rf <- function(data,formula,tree){
  result <- NA
  result1 <- NA
  for (i in tree) {
    result <- rbind(result,fn.boots(data,formula,boots=10,"rf",ntrees = i)[[3]])
    result1 <- rbind(result1,fn.cv(data,formula,k=10,"rf",ntrees = i)[[3]])
  }
  return(data.frame(para=tree,boots=result[-1,],cv=result1[-1,]))
  
}

# para.rf.hys <- fn.para.rf(data = data.gb.hys,formula = hysteresis~.,tree=c(10,20,50,seq(from=100,to=1500,by=100)))
# para.rf.enth <- fn.para.rf(data = data.gb.enth,formula = enthalpy~.,tree=c(10,20,50,seq(from=100,to=1500,by=100)))

## gaussian process
fn.para.gp <- function(data,formula,param){
  result <- NA
  result1 <- NA
  for (i in 1:nrow(param)) {
    result <- rbind(result,fn.boots(data,formula,boots=10,"gp",gp = param[i,])[[3]])
    result1 <- rbind(result1,fn.cv(data,formula,k=10,"gp",gp = param[i,])[[3]])
  }
  return(data.frame(para=param,boots=result[-1,],cv=result1[-1,]))
  
}

# gp.para <- expand.grid(noise=c(0.01,0.05,0.1,0.5,1,2,5,10),lower=c(0.001,0.01,0.1),upper=c(0.5,1,10))
# 
# para.gp.hys <- fn.para.gp(data = data.gb.hys,formula = hysteresis~.,param = gp.para)
# para.gp.enth <- fn.para.gp(data = data.gb.enth,formula = enthalpy~.,param = gp.para)


## gradient boosting
fn.para.gbm <- function(data,formula,param){
  result <- NA
  result1 <- NA
  for (i in 1:nrow(param)) {
    result <- rbind(result,fn.boots(data,formula,boots=10,"gbm",gbm  = param[i,])[[3]])
    result1 <- rbind(result1,fn.cv(data,formula,k=10,"gbm",gbm = param[i,])[[3]])
  }
  return(data.frame(para=param,boots=result[-1,],cv=result1[-1,]))
  
}


# gbm.para <- expand.grid(n.trees = c(10,20,50,100,200,300,400,500),depth = c(1:7),shrinkage = c(0.01,0.1,1))
# gbm.para <- as.matrix(gbm.para)
# para.gbm.hys <- fn.para.gbm(data = data.gb.hys,formula = hysteresis~.,param = gbm.para)
# para.gbm.enth <- fn.para.gbm(data = data.gb.enth,formula = enthalpy~.,param = gbm.para)

# plot(x=c(1:nrow(gbm.para)),y=para.gbm.hys$boots,col="red")
# points(x=c(1:nrow(gbm.para)),y=para.gbm.hys$cv,col="blue")
# min(para.gbm.hys$boots+para.gbm.hys$cv)
# max(abs(para.gbm.hys$boots-para.gbm.hys$cv))





# vir.test <- fn.virtualdata()
fn.boots.vir <- function(data,test,formula,boots,method,
                         svr=c(1,4),rf=100,gp=c(1,0.1,2),gbm=c(1000,6,0.1),xgb=c(5,30)){
  
  # ID <- test[,1]
  # test <- test[,-1]
  # test <- test[,-c(1,ncol(test))] # delete the ID column and hysteresis NA
  n <- nrow(data)
  # res.boots <- matrix(nrow=nrow(data))
  res.boots <- NA
  # colnames(res.boots) <- NULL
  for (i in 1:boots) {
    set.seed(111111+i)
    samp <- sample(n,n,replace=T)
    if(method=="svr.rbf")
      res.boots <- cbind(res.boots,fn.svr.rbf(data[samp,],test,formula,svr))
    else if(method=="rf")
      res.boots <- cbind(res.boots,fn.rf(data[samp,],test,formula,rf))
    else if(method=="gbm")
      res.boots <- cbind(res.boots,fn.gbm(data[samp,],test,formula,gbm))
    else if(method=="gp")
      res.boots <- cbind(res.boots,fn.gp(data[samp,-ncol(data)],data[samp,ncol(data)],test)$mean,gp)
    else if(method=="xgb")
      res.boots <- cbind(res.boots,fn.xgboost(data[samp,],test,formula,xgb))
    print(i)
    
  }
  colnames(res.boots) <- NULL
  res.boots <- res.boots[,-1]
  # return(res.boots)
  sd <- apply(res.boots,1,sd)
  mean <- rowMeans(res.boots)# mean predicted values
  # rmse <- sqrt(sum((mean-data[,ncol(data)])^2)/nrow(data))
  
  # res.boots$mean <- mean
  # res.boots$sd <- sd
  result <- data.frame(ID=c(1:nrow(test)),mean,sd)
  #res.boots$rmse <- rmse
  # return(list(res.boots,rmse))
  # return(list(res.boots,result))
  return(result)
}



fn.pred.vir <- function(data,test,formula,method,
                         svr=c(1,4),rf=100,gp=c(1,0.1,2),gbm=c(1000,6,0.1),xgb=c(5,30)){
  
  # ID <- test[,1]
  # test <- test[,-1]
  # test <- test[,-c(1,ncol(test))] # delete the ID column and hysteresis NA
  n <- nrow(data)
  # res.boots <- matrix(nrow=nrow(data))
  # res.boots <- NA
  # colnames(res.boots) <- NULL
    set.seed(111111)
    samp <- sample(n,n,replace=T)
    if(method=="svr.rbf")
      res.boots <- fn.svr.rbf(data,test,formula,svr)
    else if(method=="rf")
      res.boots <- fn.rf(data,test,formula,rf)
    else if(method=="xgb")
      res.boots <- fn.xgboost(data,test,formula,xgb)

  # rmse <- sqrt(sum((mean-data[,ncol(data)])^2)/nrow(data))
  
  # res.boots$mean <- mean
  # res.boots$sd <- sd
  #res.boots$rmse <- rmse
  # return(list(res.boots,rmse))
  # return(list(res.boots,result))
  return(res.boots)
}











# fn.boots.vir <- function(data,test,formula,boots,method){
#   
#   ID <- test[,1]
#   # test <- test[,-c(1,ncol(test))] # delete the ID column and hysteresis NA
#   test <- test[,-1]# delete the ID column
#   n <- nrow(data)
#   # res.boots <- matrix(nrow=nrow(data))
#   res.boots <- NA
#   # colnames(res.boots) <- NULL
#   for (i in 1:boots) {
#     set.seed(111111+i)
#     samp <- sample(n,n,replace=T)
#     if(method=="svr.rbf")
#       res.boots <- cbind(res.boots,fn.svr.rbf(data[samp,],test,formula))
#     else if(method=="rf")
#       res.boots <- cbind(res.boots,fn.rf(data[samp,],test,formula))
#     else if(method=="gbm")
#       res.boots <- cbind(res.boots,fn.gbm(data[samp,],test,formula))
#     else if(method=="gp")
#       res.boots <- cbind(res.boots,fn.gp(data[samp,-ncol(data)],data[samp,ncol(data)],test))
# 
#     print(i)
#     
#   }
#   # colnames(res.boots) <- NULL
#   res.boots <- res.boots[,-1]
#   sd <- apply(res.boots,1,sd)
#   mean <- rowMeans(res.boots)# mean predicted values
# 
#   result <- data.frame(ID,mean,sd)
# 
#   return(result)
# }

fn.cv.best.subset = function(data,n,method,formula,start=1,end=5){
  
  features <- data[,-ncol(data)]
  response <- data[,ncol(data)]
  
  # input <- cbind(features,input.response)
  # test.response <- as.data.frame(virtual[,ncol(virtual)])
  namelist <- colnames(features)
  result <- list()
  for (i in start:end){
    subset <- t(combn(x=namelist,m=i))
    rmse <- c()
    for(j in 1:nrow(subset)){
      # j=6
      input.features <- as.data.frame(features[,subset[j,]])
      colnames(input.features) <- subset[j,]
      input.response <- response
      # test.features <- as.data.frame(virtual[,subset[j,]])
      # colnames(test.features) <- subset[j,]
      input <- cbind(input.features,response=input.response)
      formula <- response~.
      if(method=="gp"){
        prediction <- fn.cv(input,formula,n,"gp")
      }
      if(method=="svr.rbf"){
        prediction <- fn.cv(input,formula,n,"svr.rbf")
      }
      if(method=="rf"){
        prediction <- fn.cv(input,formula,n,"rf")
      }
      if(method=="gbm"){
        prediction <- fn.cv(input,formula,n,"gbm")
      }
      rmse[j] <- prediction[[2]]
    }
    print(paste(i,"features has been searched out."))
    result[[i]] <- data.frame(subset,rmse)
  }
  return(result)
}




## functions parameter decision
fn.para.svr.decision <- function(data,formula,k=10,boots=100,method="svr.rbf",paralist){
  
  # cv.res <- fn.cv.loop(data,formula,k,method,loop=10,svr=paralist)
  # cv.rmse <- mean(cv.res$rmse)
  # 
  # boots.res <- fn.boots(data,formula,b,method,svr=paralist)
  # boots.rmse <- mean(boots.res$rmse)
  
  prediction <- fn.boots.cv(data,formula,boots=boots,k,method,svr=paralist)
  # rmse <- mean(prediction$rmse)
  rmse <- fn.rmse(prediction[,1:boots],data[,ncol(data)])
  R2 <- fn.R2(prediction[,1:boots],data[,ncol(data)])
  result <- cbind(paralist,rmse=rmse[[2]],rsd=rmse[[3]],R2=R2[[2]],R2sd=R2[[3]])
  return(result)
}



fn.para.rf.decision <- function(data,formula,k=10,boots=100,method="rf",paralist){
  # cv.mae <- NA
  # cv.res <- fn.cv.loop(data,formula,k,method,loop=10,ntrees=paralist)
  # cv.rmse <- mean(cv.res$rmse)
  # 
  # boots.res <- fn.boots(data,formula,b,method,ntrees=paralist)
  # boots.rmse <- mean(boots.res$rmse)
  prediction <- fn.boots.cv(data,formula,boots=boots,k,method,rf=paralist)
  rmse <- fn.rmse(prediction[,1:boots],data[,ncol(data)])
  R2 <- fn.R2(prediction[,1:boots],data[,ncol(data)])
  result <- cbind(paralist,rmse=rmse[[2]],rsd=rmse[[3]],R2=R2[[2]],R2sd=R2[[3]])
  return(data.frame(result))
}


fn.para.gp.decision <- function(data,formula,k=10,boots=100,method="gp",paralist){
  
  # cv.res <- fn.cv.loop(data,formula,k,method,loop=10,gp=paralist)
  # cv.rmse <- mean(cv.res$rmse)
  # boots.res <- fn.boots(data,formula,b,method,gp=paralist)
  # boots.rmse <- mean(boots.res$rmse)
  prediction <- fn.boots.cv(data,formula,boots=boots,k,method,gp=paralist)
  rmse <- fn.rmse(prediction[,1:boots],data[,ncol(data)])
  R2 <- fn.R2(prediction[,1:boots],data[,ncol(data)])
  result <- cbind(paralist,rmse=rmse[[2]],rsd=rmse[[3]],R2=R2[[2]],R2sd=R2[[3]])
  return(result)
}


fn.para.gbm.decision <- function(data,formula,k=10,boots=100,method="gbm",paralist){
  para <- as.matrix(paralist)
  # cv.res <- fn.cv.loop(data,formula,k,method,loop=10,gbm=para)
  # cv.rmse <- mean(cv.res$rmse)
  # boots.res <- fn.boots(data,formula,b,method,gbm=para)
  # boots.rmse <- mean(boots.res$rmse)
  
  prediction <- fn.boots.cv(data,formula,boots=boots,k,method,gbm=para)
  rmse <- fn.rmse(prediction[,1:boots],data[,ncol(data)])
  R2 <- fn.R2(prediction[,1:boots],data[,ncol(data)])
  result <- cbind(paralist,rmse=rmse[[2]],rsd=rmse[[3]],R2=R2[[2]],R2sd=R2[[3]])
  return(result)
}

fn.para.xgb.decision <- function(data,formula,k=10,boots=100,method="xgb",paralist){
  paralist <- as.matrix(paralist)
  prediction <- fn.boots.cv(data,formula,boots=boots,k,method,xgb=paralist)
  # rmse <- mean(prediction$rmse)
  rmse <- fn.rmse(prediction[,1:boots],data[,ncol(data)])
  R2 <- fn.R2(prediction[,1:boots],data[,ncol(data)])
  result <- cbind(paralist,rmse=rmse[[2]],rsd=rmse[[3]],R2=R2[[2]],R2sd=R2[[3]])
  return(result)
}


# fn.paretoFront <- function(f1,f2,ID,min=T){
#   
#   if(min){
#     d = data.frame(f1,f2,ID)
#     D = d[order(d$f1,d$f2,decreasing=FALSE),]
#     front = D[which(!duplicated(cummin(D$f2))),] # cummin = search for the min appearing
#     front = front[order(front$ID,decreasing = F),]
#   }
#   else{
#     d = data.frame(f1,f2,ID)
#     D = d[order(d$f1,d$f2,decreasing=TRUE),] # decreasing order by x, so the first x is max
#     front = D[which(!duplicated(cummax(D$f2))),] # cummax = search for the max appearing until that position
#     front = front[order(front$ID,decreasing = F),]
#   }
#   return(front$ID)
# } 



# [1] "S.mix"                   "r.atom.mix"              "delta.r.mix"             "mismatch.x.mix"         
# [5] "lamda.mix"               "Tm.mix"                  "x.mix"                   "delta.x.mix"            
# [9] "H.mix"                   "ou.mix"                  "ven.mix"                 "ea.mix"                 
# [13] "energy.cohesive.mix"     "ym.mix"                  "delta.ym.mix"            "Poisson.Ratio.mix"      
# [17] "delta.Poisson.Ratio.mix" "cs.mix"                  "delta.cs.mix"            "rm.mix"                 
# [21] "delta.rm.mix"            "bm.mix"                  "delta.bm.mix"            "Efus.mix"               
# [25] "delta.Efus.mix"          "s.solid.mix"             "delta.s.solid.mix"       "Debye.mix"              
# [29] "delta.Debye.mix"         "enth.vap.mix"            "delta.enth.vap.mix"      "enth.atom.mix"          
# [33] "delta.enth.atom.mix"     "wf.mix"                  "delta.wf.mix"            "amass.mix"              
# [37] "delta.amass.mix"         "conT.mix"                "delta.conT.mix"          "cm.mix"                 
# [41] "delta.cm.mix"            "cp.mix"                  "delta.cp.mix"            "m.hcap.mix"             
# [45] "delta.m.hcap.mix"        "cne.mix"                 "delta.cne.mix"           "hmo.mix"                
# [49] "delta.hmo.mix"           "hom.mix"                 "delta.hom.mix"           "enth.vac.mix"           
# [53] "delta.enth.vac.mix"      "mcs.mix"                 "delta.mcs.mix"           "vp.mix"                 
# [57] "delta.vp.mix"            "vatom.mix"               "delta.vatom.mix"        






