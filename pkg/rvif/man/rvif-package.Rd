\encoding{latin1}
\name{rvif-package}
\alias{rvif-package}
\alias{rvif}
\docType{package}
\title{
  Multicollinearity Detection using RVIF and graphical methods
}
\description{
The detection of troubling near multicollinearity in a multiple linear regression model is a classical problem in Econometrics. The purpose of this package is its detection by using the Redefined Variance Inflation Factor (RVIF) and the scatterplot between the Variance Inflation Factor (VIF) and the Coefficient of Variation (CV).
}
\details{
This package contains two functions. On the one hand, CV_VIF, provides the values of the Variance Inflation Factor (VIF) and the Coefficient of Variation (CV), as well as its representation in a scatter plot. Taking into account that the VIF is useful for detecting essential multicollinearity and the CV is useful for detecting non-essential multicollinearity, the scatter plot of both measures can provide interesting information for detecting whether there is a troubling degree of multicollinearity, what kind of multicollinearity it is and which variables are causing the multicollinearity.

On the other hand, RVIF, calculate the redefined VIF, the percentage of near multicollinearity due to each independent variable and, using the above function, the catter plot between the CV and VIF.
}
\author{
  Rom�n Salmer�n G�mez (University of Granada) and Catalina Garc�a Garc�a (University of Granada).

  Maintainer: Rom�n Salmer�n G�mez (romansg@ugr.es)
}
\references{
R. Salmer�n, C. Garc�a, and J. Garc�a. Variance inflation factor and condition number in multiple linear
regression. Journal of Statistical Computation and Simulation, 88:2365-2384, 2018.

R. Salmer�n, A. Rodr�guez, and C. Garc�a. Diagnosis and quantification of the non-essential collinearity.
Computational Statistics, 35:647-666, 2020.

Limitations in Detecting Multicollinearity due to Scaling Issues in the mcvis Package by Salmer�n, R., Garc�a, C.B, Rodr�guez, A. and Garc�a, C. (working paper).
  
A redefined VIF by Salmer�n, R., Garc�a, C.B, Garc�a, J. (working paper).
}

\keyword{Variation Inflation Factor Redefined}
\keyword{Variation Inflation Factor}
\keyword{Coefficient of Variation}

