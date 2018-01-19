print.Gcomputation<-function(x, digits=4, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation")){
    cat("\n")
    cat(paste0("Call: TimeVaryGcomp(model = ",paste(object$model)[2]," ",paste(object$model)[1]," ",paste(object$model)[3], ","),"\n")
    for(i in 1:length(object$cond))
      cat(paste0(if(i==1)"                    cond = c(" else "                             ",
                 paste(object$cond[[i]])[2]," ",
                 paste(object$cond[[i]])[1]," ",
                 paste(object$cond[[i]])[3],if(i==length(object$cond))(")," )else ","),
          "\n")
    if(object$missingObs)cat(paste0("                    missingObs = ",object$missingObs,","),"\n")
    cat(paste0("                    data = ",object$Namedata,")\n\n"))
    print(round(object$coefficients,digits))
  }
}
