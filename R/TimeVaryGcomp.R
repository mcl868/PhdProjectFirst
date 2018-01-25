TimeVaryGcomp<-function(model, cond, data, missingObs = FALSE, family = gaussian()){
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

  result$cond<-cond
  result$missingObs<-missingObs
  result$data<-fulldata
  result$NBallObs<-nrow(fulldata)
  result$NBestObs<-nrow(DataMis)
  result$NonMis<-colSums(!is.na(DataMis))
  
  for(i in 1:length(cond)){
    tempvalues<-predict(glm(cond[[i]],data = DataMis),type="response",newdata=fulldata)
    eval(parse(text = paste0("fulldata$",all.vars(cond[[i]])[1],"<-tempvalues")))}

  betahat<-matrix(glm(model,data = fulldata)$coefficients[-1])
  colnames(betahat)<-"Estimat"
  rownames(betahat)<-all.vars(model)[-1]
  result$coefficients<-t(betahat)
  
  attr(result,"class")<-"Gcomputation"
  out<-structure(result, class = "Gcomputation")
  
  return(out)
}
