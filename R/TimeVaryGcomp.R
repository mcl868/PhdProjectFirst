TimeVaryGcomp<-function(model, cond, data, family=gaussian()){
  new.data<-data

  result<-list(model=model)
  result$data<-data
  
  for(i in 1:length(cond)){
    tempvalues<-predict(glm(cond[[i]],data = data),type="response",newdata=new.data)
    eval(parse(text = paste0("new.data$",all.vars(cond[[i]])[1],"<-tempvalues")))}

  betahat<-matrix(glm(model,data = new.data)$coefficients[-1])
  colnames(betahat)<-"Estimat"
  rownames(betahat)<-all.vars(model)[-1]
  result$betahat<-betahat
  
  attr(result,"class")<-"Gcomputation"
  out<-structure(result, class = "Gcomputation")
  
  return(out)
}
