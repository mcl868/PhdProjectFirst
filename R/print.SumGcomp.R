print.SumGest<-function(x, digits=4, ...){
  object<-x
  rm(x)
  if(inherits(object,"SumGest")){
    cat("\n")
    cat(paste0("Call: TimeVaryGest(model = ",paste(object$model)[2]," ",paste(object$model)[1]," ",paste(object$model)[3], ","),"\n")
    for(i in 1:length(object$cond))
      cat(paste0(if(i==1)"                   cond = c(" else "                            ",
                 paste(object$cond[[i]])[2]," ",
                 paste(object$cond[[i]])[1]," ",
                 paste(object$cond[[i]])[3],if(i==length(object$cond))(")," )else ","),
          "\n")
    if(object$missingObs)cat(paste0("                    missingObs = ",object$missingObs,","),"\n")
    if(!object$nboot==100)cat(paste0("                    nboot = ",object$nboot,",\n"))
    cat(paste0("                    data = ",object$Namedata,")\n\n"))
    cat("Coefficients:         95%-Conf.Int.\n")
    print(round(object$coefficients,digits))
    cat("\n")
    cat("The size of the dataset used to estimtation:",object$NBestObs,"\n")
    if(object$missingObs){
      cat("The size of the dataset with out any missing observations:",object$NBallObs,"\n")
      cat("The distribution of observed observations:\n")
      print(round(object$NonMis))
    }
  }
}