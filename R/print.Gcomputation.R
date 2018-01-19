print.Gcomputation<-function(x, digits=4, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation")){
    cat("\n")
    print(paste0("Call: TimeVaryGcomp(model = ",paste(object$model)[2]," ",paste(object$model)[1]," ",paste(object$model)[3], ","))
    for(i in 1:length(object$cond))
    print(paste0(if(i==1)"cond = c(" else "         ",
      paste(object$cond[[i]])[2]," ",paste(object$cond[[i]])[1]," ",paste(object$cond[[i]])[3],if(i==length(object$cond))")" else ""))
    cat("\n")
    print(object$coefficients)
    }
}
