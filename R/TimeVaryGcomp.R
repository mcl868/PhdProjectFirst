TimeVaryGcomp<-function(model, cond, data, missingObs=FALSE, family=gaussian()){
  if(missingObs){
    data$Nmis<-eval(parse(text=paste0("is.na(data$",colnames(data),")",collapse = "+")))
    DataMis<-data[data$Nmis<=missingObs,]
    fulldata<-data[eval(parse(text=paste0("!is.na(data$",colnames(data),")",collapse = " & "))),]
    fulldata$Nmis<-NULL
  } else {
    fulldata<-DataMis<-data[eval(parse(text=paste0("!is.na(data$",colnames(data),")",collapse = " & "))),]
  }

  result<-list(model=model)
  result$data<-fulldata
  
  for(i in 1:length(cond)){
    tempvalues<-predict(glm(cond[[i]],data = fulldata),type="response",newdata=DataMis)
    eval(parse(text = paste0("DataMis$",all.vars(cond[[i]])[1],"<-tempvalues")))}

  betahat<-matrix(glm(model,data = DataMis)$coefficients[-1])
  colnames(betahat)<-"Estimat"
  rownames(betahat)<-all.vars(model)[-1]
  result$betahat<-betahat
  
  attr(result,"class")<-"Gcomputation"
  out<-structure(result, class = "Gcomputation")
  
  return(out)
}
