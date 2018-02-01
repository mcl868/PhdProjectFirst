TimeVaryGcomp<-function(model, cond, data, nboot=100, missingObs = FALSE, family = gaussian(), ...){
  result<-list(model=model)
  result$Namedata<-deparse(substitute(data))

  if(missingObs){
    data$Nmis<-eval(parse(text=paste0("is.na(data$",all.vars(cond[[length(cond)]]),")",collapse = "+")))
    DataMis<-data[data$Nmis<=missingObs,]
    fulldata<-data[eval(parse(text=paste0("!is.na(data$",all.vars(cond[[length(cond)]]),")",collapse = " & "))),]
    DataMis$Nmis<-NULL
    fulldata$Nmis<-NULL
  } else {
    fulldata<-DataMis<-data[eval(parse(text=paste0("!is.na(data$",all.vars(cond[[length(cond)]]),")",collapse = " & "))),]
  }

  result$cond<-cond
  result$missingObs<-missingObs
  result$data<-fulldata
  result$Estdata<-DataMis
  result$NBallObs<-nrow(fulldata)
  result$NBestObs<-nrow(DataMis)
  result$NonMis<-colSums(!is.na(DataMis))
  result$nboot<-nboot
  
  for(i in 1:length(cond)){
    GenLinModel<-glm(cond[[i]],data = DataMis, family = family)
    tempRes<-GenLinModel$residuals
    tempvalues<-predict(GenLinModel,type="response",newdata=fulldata)
    eval(parse(text = paste0("fulldata$",all.vars(cond[[i]])[1],"<-tempvalues")))
    eval(parse(text = paste0("result$Residuals$Res",all.vars(cond[[i]])[1],"<-tempRes")))}

  betahat<-matrix(glm(model,data = fulldata, family = family)$coefficients[-1])
  colnames(betahat)<-"Estimat"
  rownames(betahat)<-all.vars(model)[-1]
  result$coefficients<-t(betahat)
  
  attr(result,"class")<-"Gcomputation"
  out<-structure(result, class = "Gcomputation")
  
  return(out)
}
