\name{TimeVaryGcomp}
\alias{TimeVaryGcomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
TimeVaryGcomp
}
\description{
  A concise (1-5 lines) description of what the function does. 
}
\usage{
TimeVaryGcoomp(model, cond, data, missingObs = FALSE, family=gaussian())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{x} here~~
}
  \item{cond}{
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
N<-2000
set.seed(27)
A0<-rnorm(N)
L0<-2*A0+rnorm(N)
A1<-3*L0+2.4*A0+rnorm(N)
Y<-2*A0+3*A1+rnorm(N)
DAG<-data.frame(Y,A0,A1,L0)
rm(list=c("Y","A0","A1","L0"))

TimeVaryGcomp(model<-Y ~ A0 + A1,
              cond=c(L0~A0),
              data=DAG,
              family=gaussian())

Call: TimeVaryGcomp(model = Y ~ A0 + A1, 
                    cond = c(L0 ~ A0), 
                    data = DAG)

Coefficients:
            A0     A1
Estimat 1.9603 3.0066
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
