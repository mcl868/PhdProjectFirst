A0<-rnorm(1000)
L<-2*A0+rnorm(1000)
A1<-3*L+2.4*A0+rnorm(1000)
Y<-2*A0+3*A1+rnorm(1000)

A0hat<-predict(glm(A0~1),type = "response")

(A0-A0hat)
hej