\name{summary.Gcomputation}
\alias{summary.Gcomputation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print summary statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.Gcomputation(x, digist = 4, ...)
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
  \item{Method}{
Tells how the Bandwidth-object is created. This method will marked as folliwing: "Bootstrap" or "Cross validation".
}
  \item{Curve}{
----
}
  \item{y.values}{
If \code{values_y} is missing in the statment then a sequence of a 51 grid that start from the 0.5 percent quantile of \code{Yseq} and ends with the 99.5 percent quantile of \code{Yseq} otherwise the sequence in the statement.
}
  \item{h.values}{
Sequence of h values.
}
  \item{min.h}{
The \code{h} of the sequence \code{values_h} that minimizes the curve of bootstrap.
}
  \item{min}{
The minimum of the bootstrap or "Cross validation" curve.
}
  \item{max}{
The maximum of the bootstrap or "Cross validation" curve.
}
  \item{lower}{
The value of the 2.5 percent quantile of \code{Xseq} otherwise the sequence in the statement.
}
  \item{upper}{
The value of the 97.5 percent quantile of \code{Xseq} otherwise the sequence in the statement.
}
  \item{Data.Frame}{
--
}
The following applies only for the bootstrap-method:
  \item{Sigma}{
The value of \eqn{\sigma} for the particular model. Look at \code{\link{Bootstrap}} for further information.
}
  \item{coef}{
The values of coefficients of the polynumium for the particular model. Look at \code{\link{Bootstrap}} for further information.
}
  \item{hatplugIn}{
If a kernel \code{W} is defined in the workspace without using the function \code{Kernelobj} (See \code{\link{Kernelobj}} for further information) then you will not obtain the value of the plug-in \eqn{\hat{h}}. If not kernel \code{W} is defined in the workspace at all then by DEFAULT you will obtain the plug-in \eqn{\hat{h}} because the standard normal distribution is picked as the kernel \code{W}. See \code{\link{plugin.h}} for further information.
}
  \item{alpha_star}{
It applies the same option here as for \code{hatplugIn}.
}
  \item{delta_star}{
It applies the same option here as for \code{hatplugIn}.
}
  \item{kappa2}{
It applies the same option here as for \code{hatplugIn}.
}
  \item{tau.sq}{
It applies the same option here as for \code{hatplugIn}.
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
b.h<-Bootstrap(Yseq=OU_251[[1]],
               values_h=seq(0.1,0.32,length.out=5),
               y.grid=11,
               NBoot=2)
b.h
summary(b.h)}

\dontrun{
cv.h<-CrossVMulti(Yseq=CIR_251[[1]],
                  values_h=seq(0.1,1,length.out=17),
                  y.grid=19)
cv.h
summary(cv.h)}
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
