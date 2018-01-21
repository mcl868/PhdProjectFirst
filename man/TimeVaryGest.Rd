\name{TimeVaryGest}
\alias{TimeVaryGest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TimeVaryGest
}
\description{
  A concise (1-5 lines) description of what the function does. 
}
\usage{
TimeVaryGest(model, treatment, missingObs, data, family=gaussian())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{x} here~~
}
  \item{treatment}{
%%     ~~Describe \code{x} here~~
}
  \item{missingObs}{
%%     ~~Describe \code{x} here~~
}
  \item{data}{
%%     ~~Describe \code{x} here~~
}
  \item{family}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
put references to the literature/web site here
}
\author{
Thomas Maltesen
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{
set.seed(3)
N<-2000
A0<-rnorm(N)
L0<-2*A0+rnorm(N)
A1<-3*L0+2.4*A0+rnorm(N)
L1<-3*A1+rnorm(N)
A2<-3*L1+2.4*A1+rnorm(N)
Y<-2*A0+3*A1+4*A2+rnorm(N)
DAG<-data.frame(Y,A0,A1,A2,L0,L1)
rm(list=c("Y","A0","A1","A2","L0","L1"))

TimeVaryGest(model<-Y ~ A0 + A1 + A2,
             treatment=c(A0~1,
                         A1~A0+L0,
                         A2~A1+L1),
             data=DAG,
             family=gaussian())

Call: TimeVaryGest(model = Y ~ A0 + A1 + A2, 
                    cond = c(A0 ~ 1, 
                             A1 ~ A0 + L0, 
                             A2 ~ A1 + L1, 
                    data = DAG)

            A0     A1     A2
Estimat 1.9774 3.5542 3.9515

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
