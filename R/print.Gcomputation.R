print.Gcomputation<-function(x, digits=4, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation")){
    cat("\n")
    cat("Model:\n")
    print(object$model)
    cat("\n")
    print(object$betahat)
    }
}
