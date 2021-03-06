bootstrapTimeVary<-function(x, n, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation") | inherits(object,"Gestimation")){
    booo<-lapply(1:n,function(i)sample(c(1:object$NBestObs),object$NBestObs,replace=TRUE))
      if(inherits(object,"Gcomputation")){
        boot<-lapply(1:n,function(i)
          TimeVaryGcomp(model=object$model,cond=object$cond,
                        missingObs=object$missingObs,
                        data=object$Estdata[booo[[i]],])$coefficients)
      }
      if(inherits(object,"Gestimation")){
        boot<-lapply(1:n,function(i)
          TimeVaryGest(model=object$model,cond=object$cond,
                        missingObs=object$missingObs,
                        data=object$Estdata[booo[[i]],])$coefficients)
      }
  }
  result<-listVar(boot)
  return(result)
}
