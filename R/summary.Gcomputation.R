summary.Gcomputation<-function(x, ...){
  object<-x
  rm(x)
  if(inherits(object,"Gcomputation")){
    xboot<-bootstrapTimeVary(object, object$nboot)
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
  attr(result, "class") <- "SumGcomp"
  out <- structure(result, class = "SumGcomp")
  return(out)
}
