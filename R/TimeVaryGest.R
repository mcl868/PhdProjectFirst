TimeVaryGest<-function(model, cond, data, nboot=100, missingObs = FALSE, family = gaussian(), ...){
  result<-list(model=model)
  result$Namedata<-deparse(substitute(data))

  if(missingObs){
    data$Nmis<-eval(parse(text=paste0("is.na(data$",colnames(data),")",collapse = "+")))
    DataMis<-data[data$Nmis<=missingObs,]
    fulldata<-data[eval(parse(text=paste0("!is.na(data$",colnames(data),")",collapse = " & "))),]
    DataMis$Nmis<-NULL
    fulldata$Nmis<-NULL
  } else {
    fulldata<-DataMis<-data[eval(parse(text=paste0("!is.na(data$",colnames(data),")",collapse = " & "))),]
  }
 
  for(i in 1:length(cond)){
    tempRes<-glm(cond[[i]],data=DataMis,family=family)$residuals
    eval(parse(text = paste0("result$Residuals$Res",all.vars(cond[[i]])[1],"<-tempRes")))}

  Ahat<-lapply(1:length(cond),function(i)predict(glm(cond[[i]],data=DataMis,family=family),type = "response",newdata=fulldata))
  A<-lapply(1:length(cond),function(i)eval(parse(text=paste0("fulldata$", all.vars(formula(cond[[i]]))[1]))))
  TreatRes<-lapply(1:length(cond),function(i)A[[i]]-Ahat[[i]])
  
  Ntreat<-length(cond)
  B<-diag(Ntreat)

  result$cond<-cond
  result$missingObs<-missingObs
  result$data<-fulldata
  result$Estdata<-DataMis
  result$NBallObs<-nrow(fulldata)
  result$NBestObs<-nrow(DataMis)
  result$NonMis<-colSums(!is.na(DataMis))
  result$nboot<-nboot

#  SumMatrix<-matrix(0,Ntreat,Ntreat)
#  for(j in 1:nrow(fulldata)){
#    TemP<-matrix(0,Ntreat,Ntreat)
#    for(k in 1:Ntreat){
#      TemP<-TemP+
#      TreatRes[[k]][j]*B[,k,drop=FALSE]%*%rowSums(sapply(k:Ntreat, function(l)A[[l]][j]*B[l,,drop=FALSE]))
#        eval(parse(text=
#                     paste0(paste0("TreatRes[[",k,"]][",j,"]*B[,",k,",drop=FALSE]",collapse="+"),"%*%(",
#                            paste0("A[[",c(k:Ntreat),"]][",j,"]*B[",c(k:Ntreat),",,drop=FALSE]",
#                                   collapse="+"),")")))
#    }
#    SumMatrix<-SumMatrix+TemP
#  }
  SumMatrix<-listSums(
    lapply(1:nrow(fulldata),function(j)
      listSums(lapply(1:Ntreat, function(k)
        TreatRes[[k]][j]*B[,k,drop=FALSE]%*%rowSums(sapply(k:Ntreat, function(l) A[[l]][j]*B[l,,drop=FALSE]))))))
  result$SumMatrix<-SumMatrix

#  SumMatrixResponse<-matrix(0,Ntreat,1)
#  for(j in 1:nrow(fulldata))SumMatrixResponse<-SumMatrixResponse+
#    eval(parse(text=
#                 paste0("TreatRes[[",c(1:Ntreat),"]][",j,"]*B[,",c(1:Ntreat),",drop=FALSE]*fulldata$",
#                        all.vars(model)[1],"[",j,"]",collapse="+")))
  SumMatrixResponse <- listSums(
    lapply(1:nrow(fulldata),function(j)
      listSums(lapply(1:Ntreat, function(k)
        TreatRes[[k]][j]*B[,k,drop=FALSE]*fulldata[j,all.vars(model)[1]]))))
  result$SumMatrixResponse<-SumMatrixResponse
  
  thetahat<-solve(SumMatrix)%*%SumMatrixResponse
  colnames(thetahat)<-"Estimat"
  rownames(thetahat)<-all.vars(model)[-1]

  result$coefficients<-t(thetahat)
  
  attr(result,"class")<-"Gestimation"
  out<-structure(result, class = "Gestimation")
  
  return(out)
}
