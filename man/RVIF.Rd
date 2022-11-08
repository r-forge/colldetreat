\encoding{latin1}
\name{RVIF}
\alias{RVIF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RVIF calculation
}
\description{
This function provides the values of the Redefined Variance Inflation Factor (RVIF) and the the percentage of near multicollinearity due to each independent variable.
}
\usage{
RVIF(X, l_u=TRUE, l=40, intercept=TRUE, graf=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{X}{A numeric design matrix that should contain more than one regressor.}
  \item{l_u}{A logical value that indicates if the variables in the design matrix \code{X} are transformed to have unit length. By default \code{l_u=TRUE}.}
  \item{l}{A real number that indicates the lower limit of the vertical axis of the scatter plot between the Variance Inflation Factor (VIF) and the Coefficient of Variation (CV). By default \code{l=40}.}
  \item{intercept}{A logical value that indicates if the design matrix \code{X} have intercept. By default \code{intercept=TRUE}.}
  \item{graf}{A logical value that indicates if the scatter plot between the VIF and CV is represented by using CV_VIF function. By default \code{graf=TRUE}.}
}
\details{
The Redefined Variation Inflation Factor (RVIF) is able to detect both kind of multicollinearity: the essential (near-linear relationship between at least two independent variables excluding the intercept) an non-essential (near-linear relationship between the intercept and at least one of the remaining independent variables). This measure also quantifies the percentage of near multicollinearity due to each independent variable.
}
\value{
  \item{RVIF}{Redefined Variance Inflation Factor of each independent variable.}
  \item{\%}{Percentage of near multicollinearity due to each independent variable.}
  \item{Graph}{Scatter plot of VIF and the CV.}
}
\references{
R. Salmerón, C. García, and J. García. Variance inflation factor and condition number in multiple linear
regression. Journal of Statistical Computation and Simulation, 88:2365-2384, 2018.

R. Salmerón, A. Rodríguez, and C. García. Diagnosis and quantification of the non-essential collinearity.
Computational Statistics, 35:647-666, 2020.

A redefined VIF by Salmerón, R., García, C.B, García, J. (working paper).
}
\author{R. Salmerón (\email{romansg@ugr.es}) and C. García (\email{cbgarcia@ugr.es}).}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{CV_VIF}}}
\examples{
library(multiColl)
set.seed(2022)
obs = 100
cte = rep(1, obs)
x2 = rnorm(obs, 5, 0.01)
x3 = rnorm(obs, 5, 10)
x4 = x3 + rnorm(obs, 5, 1)
x5 = rnorm(obs, -1, 30)
x = cbind(cte, x2, x3, x4, x5)
RVIF(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Variation Inflation Factor Redefined}
\keyword{Variation Inflation Factor}
\keyword{Coefficient of Variation}