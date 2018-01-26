summary.Gestimation<-function(x, ...){
  object<-x
  rm(x)
  result<-list()
  if(inherits(object,"Gestimation")){
    result$model<-object$model
    result$Namedata<-object$Namedata
    result$cond<-object$cond
    result$missingObs<-object$missingObs
    result$data<-object$data
    result$Estdata<-object$Estdata
    result$NBallObs<-object$NBallObs
    result$NBestObs<-object$NBestObs
    result$NonMis<-object$NonMis
    result$nboot<-object$nboot

    xboot<-bootstrapTimeVary(object, object$nboot)
    NamEst<-colnames(xboot)
    EstMatrix<-matrix(NA,nrow=length(NamEst),ncol=4)
    for(i in 1:length(NamEst))
      EstMatrix[i,]<-
      cbind(object$coefficients[,NamEst[i]],
            sqrt(xboot[,NamEst[i]]),
            object$coefficients[,NamEst[i]]+qnorm(0.025)*sqrt(xboot[,NamEst[i]]),
            object$coefficients[,NamEst[i]]+qnorm(0.975)*sqrt(xboot[,NamEst[i]]))
    rownames(EstMatrix)<-NamEst
    colnames(EstMatrix)<-c("Estimate","Std. Err.","Lower","Upper")
  }
  result$Est<-EstMatrix
  attr(result, "class") <- "SumGest"
  out <- structure(result, class = "SumGest")
  return(out)
}
