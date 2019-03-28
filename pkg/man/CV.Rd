\name{CV}
\alias{CV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Coeficient of Variation}
\description{The function calculates the Coeficient of Variation (CV) of a quantitative vector.}
\usage{CV(x)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A quantitative vector.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The CV of \code{x}.}
\references{
%% ~put references to the literature/web site here ~
}
\author{Román Salmerón Gómez (\email{romansg@ugr.es}).}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{mean}}, \code{\link{var}}, \code{\link{sd}}.}
\examples{
# random
x = sample(1:50, 25)
x
CV(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{coeficient of variation}% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
