summary.Gestimation<-function(x, n=100, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gestimation")){
    xboot<-bootstrapTimeVary(object, n)
    NamEst<-colnames(xboot)
    EstMatrix<-matrix(NA,nrow=length(NamEst),ncol=4)
    for(i in 1:length(NamEst))
      EstMatrix[i,]<-
      cbind(object$coefficients[,NamEst[i]],
            sqrt(xboot[,NamEst[i]]),
            object$coefficients[,NamEst[i]]+qnorm(0.025)*sqrt(xboot[,NamEst[i]]),
            object$coefficients[,NamEst[i]]+qnorm(0.975)*sqrt(xboot[,NamEst[i]]))
  }
  result<-list(Est=EstMatrix)
  attr(result, "class") <- "SumGest"
  out <- structure(result, class = "SumGest")
  return(out)
}
