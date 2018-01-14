TimeVaryGest<-function(model, treatment, data, family=gaussian()){
  Ahat<-lapply(1:length(treatment),function(i)predict(glm(treatment[[i]],data=data,family=family),type = "response"))
  A<-lapply(1:length(treatment),function(i)eval(parse(text=paste0("data$", all.vars(formula(treatment[[i]]))[1]))))
  TreatRes<-lapply(1:length(treatment),function(i)A[[i]]-Ahat[[i]])
  
  Ntreat<-length(treatment)
  B<-diag(Ntreat)

  SumMatrix<-matrix(0,Ntreat,Ntreat)
  for(j in 1:nrow(data)){
    TemP<-matrix(0,Ntreat,Ntreat)
    for(k in 1:Ntreat){
      TemP<-TemP+
        eval(parse(text=paste0(paste0("TreatRes[[",k,"]][",j,"]*B[,",k,",drop=FALSE]",collapse="+"),"%*%(",
                               paste0("A[[",c(k:Ntreat),"]][",j,"]*B[",c(k:Ntreat),",,drop=FALSE]",collapse="+"),")")))
      
    }
    SumMatrix<-SumMatrix+TemP
  }
  
  SumMatrixResponse<-matrix(0,Ntreat,1)
  for(j in 1:nrow(data))SumMatrixResponse<-SumMatrixResponse+
    eval(parse(text=paste0("TreatRes[[",c(1:Ntreat),"]][",j,"]*B[,",c(1:Ntreat),",drop=FALSE]*data$",all.vars(model)[1],"[",j,"]",collapse="+")))
  
  return(solve(SumMatrix)%*%SumMatrixResponse)
  
}
