TimeVaryGestV2<-function(model, treatment, data, missingObs=FALSE, family=gaussian()){
  if(missingObs){
    data$Nmis<-eval(parse(text=paste0("is.na(data$",colnames(data),")",collapse = "+")))
    DataMis<-data[data$Nmis<=missingObs,]
    fulldata<-data[eval(parse(text=paste0("!is.na(data$",colnames(data),")",collapse = " & "))),]
    fulldata$Nmis<-NULL
  } else {
    fulldata<-DataMis<-data[eval(parse(text=paste0("!is.na(data$",colnames(data),")",collapse = " & "))),]
  }
 
  Ahat<-lapply(1:length(treatment),function(i)predict(glm(treatment[[i]],data=DataMis,family=family),type = "response",newdata=fulldata))
  A<-lapply(1:length(treatment),function(i)eval(parse(text=paste0("fulldata$", all.vars(formula(treatment[[i]]))[1]))))
  TreatRes<-lapply(1:length(treatment),function(i)A[[i]]-Ahat[[i]])
  
  Ntreat<-length(treatment)
  B<-diag(Ntreat)
  
  
  result<-list(model=model)
  result$data<-fulldata
  
  SumMatrix<-matrix(0,Ntreat,Ntreat)
  for(j in 1:nrow(fulldata)){
    TemP<-matrix(0,Ntreat,Ntreat)
    for(k in 1:Ntreat){
      TemP<-TemP+
        eval(parse(text=
                     paste0(paste0("TreatRes[[",k,"]][",j,"]*B[,",k,",drop=FALSE]",collapse="+"),"%*%(",
                            paste0("A[[",c(k:Ntreat),"]][",j,"]*B[",c(k:Ntreat),",,drop=FALSE]",
                                   collapse="+"),")")))
    }
    SumMatrix<-SumMatrix+TemP
  }
  result$SumMatrix<-SumMatrix

  SumMatrixResponse<-matrix(0,Ntreat,1)
  for(j in 1:nrow(fulldata))SumMatrixResponse<-SumMatrixResponse+
    eval(parse(text=
                 paste0("TreatRes[[",c(1:Ntreat),"]][",j,"]*B[,",c(1:Ntreat),",drop=FALSE]*fulldata$",
                        all.vars(model)[1],"[",j,"]",collapse="+")))
  result$SumMatrixResponse<-SumMatrixResponse
  
  thetahat<-solve(SumMatrix)%*%SumMatrixResponse
  colnames(thetahat)<-"Estimat"
  rownames(thetahat)<-all.vars(model)[-1]

  result$coefficients<-thetahat
  
  attr(result,"class")<-"Gestimation"
  out<-structure(result, class = "Gestimation")
  
  return(out)
}
