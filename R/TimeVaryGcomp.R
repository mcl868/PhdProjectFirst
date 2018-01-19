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
    tempvalues<-predict(glm(cond[[i]],data = DataMis),type="response",newdata=fulldata)
    eval(parse(text = paste0("fulldata$",all.vars(cond[[i]])[1],"<-tempvalues")))}

  betahat<-matrix(glm(model,data = fulldata)$coefficients[-1])
  colnames(betahat)<-"Estimat"
  rownames(betahat)<-all.vars(model)[-1]
  result$coefficients<-betahat
  
  attr(result,"class")<-"Gcomputation"
  out<-structure(result, class = "Gcomputation")
  
  return(out)
}
