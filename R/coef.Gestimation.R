coef.Gestimation<-function(x){
  object<-x
  rm(x)
  if(inherits(object,"Gestimation")){
    object$thetahat
    }
}
