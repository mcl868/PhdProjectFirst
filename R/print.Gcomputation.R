print.Gcomputation<-function(x, digits=4, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation")){
    cat("\n")
    print(paste0("Call: TimeVaryGcomp(model = ",paste(object$model)[2]," ",paste(object$model)[1]," ",paste(object$model)[3], ", data = "))
    cat("\n")
    print(object$coefficients)
    }
}
