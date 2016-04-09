\name{mewma}
\alias{mewma}
\title{Generates a Hotelling's T2 Graph of the Multivariate Exponentially Weighted Average}
\description{Generates a Hotelling's T2 Graph for \code{mewma} objects.}
\usage{mewma(X, phase = 1, lambda = 0.2, conf = c(0.95, 0.99), 
                      asymptotic.form = FALSE) }
\arguments{
\item{X}{ a dataframe. }
\item{phase}{ designates whether the confidence limits should reflect the current data frame, \code{phase = 1} or future observations, \code{phase = 2}. }
\item{lambda}{EWMA smoothing parameter}
\item{conf}{ the confidence level(s) to use for upper control limit. }
\item{asymptotic.form}{ use asymptotic convergence parameter for scaling the covariance matrix. }
}
\details{
\code{mewma} is used to generates a Hotelling's T2 graph for the multivariate EWMA.
}
\value{
The output of \code{mewma} is a graph of Hotelling's T2 for the Multivariate EWMS, and a list containing a data frame of univariate EWMAs and the multivariate EWMA T2 values.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{
Lowry, Cynthia A., et al. "A multivariate exponentially weighted moving average control chart." Technometrics 34.1 (1992): 46:53.
}


\examples{
mewma(iris[, -5], phase = 1, lambda = 0.2, conf = c(0.95, 0.99), 
                      asymptotic.form = FALSE)
}