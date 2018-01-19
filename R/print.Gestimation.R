print.Gestimation<-function(x, digits=4, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gestimation")){
    cat("\n")
    cat("Model:\n")
    print(object$model)
    cat("\n")
    print(object$thetahat)
    }
}
