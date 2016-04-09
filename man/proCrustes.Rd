\name{proCrustes}
\alias{proCrustes}
\alias{print.proC}
\title{Comparison of n-point Configurations vis Procrustes Analysis}
\description{Implementation of Procrustes Analysis in the spirit of multidimensional scaling.}
\usage{
proCrustes(X, Y, scaling = TRUE, standardize = FALSE, scale.unit = F, ...)
}
\arguments{
\item{X}{ Target configuration }
\item{Y}{ Matching configuration }
\item{scaling}{ Scale Y-axis }
\item{standardize}{Standardize configurations}
\item{scale.unit}{Scale to unit variance}
\item{...}{additional arguments. Currently ignored.}
}
\details{
This function implements Procrustes Analysis as described in the reference below.  That is to say:

Translation:  Fixed displacement of points through a constant distance in a common direction

Rotation:  Fixed displacement of all points through a constant angle

Dilation:  Stretching or shrinking by a contant amount

}
\value{

\item{Rotation.Matrix}{ The matrix, Q, that rotates Y towards X; obtained via \code{svd} of X'Y }
\item{Residuals}{ residuals after fitting }
\item{M2_min}{ Residual Sums of Squares }
\item{Xmeans}{ Column Means of X }
\item{Ymeans}{ Column Means of Y }
\item{PRMSE}{ Procrustes Root Mean Square Error }
\item{Yproj}{ Projected Y-values }
\item{scale}{ logical.  Should Y be scaled. }
\item{Translation}{ Scaling through a common distance based on rotation of Y and scaling parameter, c }
\item{residuals.}{ residual sum-of-squares }
\item{Anova.MSS}{ Explained Variance w.r.t. Y }
\item{Anova.ESS}{ Unexplained Variance w.r.t. Y }
\item{Anova.TSS}{ Total Sums of Squares w.r.t. X }


}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{

Krzanowski, Wojtek. Principles of multivariate analysis. OUP Oxford, 2000.


}
\examples{
X <- iris[, 1:2]
Y <- iris[, 3:4]

proc <- proCrustes(X, Y)
proc
names(proc)
}