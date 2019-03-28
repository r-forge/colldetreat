\name{PROPs}
\alias{PROPs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Proportions}
\description{The functions returns the proportion of ones in the dummy variables existing in a matrix.}
\usage{PROPs(X, dummy = T, pos = NULL)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{A numeric matrix that should contain more than one regressor (intercept included).}
  \item{dummy}{A logical value that indicates if there are dummy variables in the matrix \code{X}. By default \code{dummy=T}.}
  \item{pos}{A numeric vector that indicates the position of the dummy variables, if these exist, in the matrix \code{X}. By default \code{pos=NULL}.}
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
%% ~put references to the literature/web site here ~
}
\author{Román Salmerón Gómez (\email{romansg@ugr.es}).}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# random
x1 = sample(1:50, 25)
x2 = sample(1:50, 25)
x3 = sample(cbind(array(1,25), array(0,25)), 25)
x4 = sample(cbind(array(1,25), array(0,25)), 25)
x = cbind(x1, x2, x3, x4)
head(x)
PROPs(x, T, pos = c(3,4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Proportions}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
