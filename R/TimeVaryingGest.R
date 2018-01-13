A0<-rnorm(1000)
L<-2*A0+rnorm(1000)
A1<-3*L+2.4*A0+rnorm(1000)
L1<-3*A1+rnorm(1000)
A2<-3*L1+2.4*A1+rnorm(1000)
Y<-2*A0+3*A1+4*A2+rnorm(1000)
datFa<-data.frame(Y,A0,A1,L,A2,L1);rm(list=c("Y","A0","A1","L","A2","L1"));ls()

response<-datFa$Y

Treat0<-glm(A0~1,data=datFa)
A0hat<-predict(Treat0,type = "response")
A0Obs<-eval(parse(text=paste0("datFa$", all.vars(formula(Treat0))[1])))
Treat0res<-(A0Obs-A0hat)

Treat1<-glm(A1~A0+L,data=datFa)
A1hat<-predict(Treat1,type = "response")
A1Obs<-eval(parse(text=paste0("datFa$", all.vars(formula(Treat1))[1])))
Treat1res<-(A1Obs-A1hat)

Treat2<-glm(A2~A1+L1,data=datFa)
A2hat<-predict(Treat2,type = "response")
A2Obs<-eval(parse(text=paste0("datFa$", all.vars(formula(Treat2))[1])))
Treat2res<-(A2Obs-A2hat)


Ntreat<-3

B<-diag(Ntreat)

SumMatrix<-matrix(0,Ntreat,Ntreat)
for(j in 1:length(response))SumMatrix<-SumMatrix+
eval(parse(text=paste0("Treat",0,"res[",j,"]*B[,",1,",drop=FALSE]",collapse="+")))%*%
(eval(parse(text=paste0("A",c(1:Ntreat)-1,"Obs[",j,"]*B[",c(1:Ntreat),",,drop=FALSE]",collapse="+"))))+
eval(parse(text=paste0("Treat",1,"res[",j,"]*B[,",2,",drop=FALSE]",collapse="+")))%*%
(eval(parse(text=paste0("A",c(Ntreat:Ntreat)-1,"Obs[",j,"]*B[",c(Ntreat:Ntreat),",,drop=FALSE]",collapse="+"))))

SumMatrixResponse<-matrix(0,Ntreat,1)
for(j in 1:length(response))SumMatrixResponse<-SumMatrixResponse+
eval(parse(text=paste0("Treat",c(1:Ntreat)-1,"res[",j,"]*B[,",c(1:Ntreat),",drop=FALSE]*response[",j,"]",collapse="+")))

solve(SumMatrix)%*%SumMatrixResponse


j<-1

eval(parse(text=paste0("Treat",0,"res[",j,"]*B[,",1,",drop=FALSE]",collapse="+")))%*%
  (eval(parse(text=paste0("A",c(1:Ntreat)-1,"Obs[",j,"]*B[",c(1:Ntreat),",,drop=FALSE]",collapse="+"))))
  




SumMatrix<-matrix(0,Ntreat,Ntreat)
for(j in 1:length(response))SumMatrix<-SumMatrix+
  eval(parse(text=paste0("Treat",0,"res[",j,"]*B[,",1,",drop=FALSE]",collapse="+")))%*%
  (eval(parse(text=paste0("A",c(1:Ntreat)-1,"Obs[",j,"]*B[",c(1:Ntreat),",,drop=FALSE]",collapse="+"))))+
  eval(parse(text=paste0("Treat",1,"res[",j,"]*B[,",2,",drop=FALSE]",collapse="+")))%*%
  (eval(parse(text=paste0("A",c(Ntreat:Ntreat)-1,"Obs[",j,"]*B[",c(Ntreat:Ntreat),",,drop=FALSE]",collapse="+"))))

