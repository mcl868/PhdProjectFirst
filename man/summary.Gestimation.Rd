\name{summary.Gestimation}
\alias{summary.Gestimation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print summary statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.Gestimation(x, digist = 4, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{digist}{
%%     ~~Describe \code{digist} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{model}{
----
}
  \item{Namedata}{
----
}
  \item{cond}{
----
}
  \item{missingObs}{
----
}
  \item{data}{
----
}
  \item{Estdata}{
----
}
  \item{NBallObs}{
----
}
  \item{NBestObs}{
----
}
  \item{NonMis}{
----
}
  \item{nboot}{
----
}
  \item{SumMatrix}{
----
}
  \item{SumMatrixResponse}{
----
}
  \item{coefficients}{
----
}
}
\references{
%% ~put references to the literature/web site here ~
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
set.seed(3)
A0<-rnorm(N)
L0<-2*A0+rnorm(N)
A1<-3*L0+2.4*A0+rnorm(N)
Y<-2*A0+2*L0+3*A1+rnorm(N)
DAG.2<-data.frame(Y,A0,A1,L0)
rm("Y", "A0", "A1", "L0")

G<-
TimeVaryGest(model=Y ~ A0 + A1,
             cond=c(A0~1,
                    A1~A0+L0),
             data=DAG.2)
G
summary(G)

Call: TimeVaryGest(model = Y ~ A0 + A1, 
                   cond = c(A0 ~ 1, 
                            A1 ~ A0 + L0), 
                    data = DAG.2)

Coefficients:         95%-Conf.Int.
   Estimate Std. Err.  Lower  Upper
A0   5.7444    0.1888 5.3744 6.1144
A1   3.0342    0.0217 2.9917 3.0767

The size of the dataset used to estimtation: 2000}
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
