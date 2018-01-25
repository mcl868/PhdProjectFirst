bootstrapTimeVary<-function(x,n=1000){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation") | inherits(object,"Gestimation")){
    object$data
    object$Estdata
    object$NBallObs
    object$NBestObs
    boot<-list()
    for(ii in 1:n){
      print(ii)
      booo<-sample(c(1:object$NBestObs),object$NBestObs,replace=TRUE)
      if(inherits(object,"Gcomputation")){
      boot[[ii]]<-TimeVaryGcomp(model=object$model,cond=object$cond,missingObs=object$missingObs,data=object$Estdata[booo,])$coefficients
      }
      if(inherits(object,"Gestimation")){
      boot[[ii]]<-TimeVaryGest(model=object$model,cond=object$cond,missingObs=object$missingObs,data=object$Estdata[booo,])$coefficients
      }
    }
  }
  return(boot)
}

