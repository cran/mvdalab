\name{plusminus.loo}
\alias{plusminus.loo}
\title{Leave-one-out routine for \code{plusminus} objects}
\description{When \code{validation = 'loo'} this routine effects the leave-one-out cross-validation procedure for \code{plusminus} objects.}
\usage{
plusminus.loo(X, Y, method = "plusminus", n_cores, ...)
}
\arguments{
\item{X}{ a matrix of observations. \code{NAs} and Infs are not allowed. }
\item{Y}{ a vector. \code{NAs} and \code{Infs} are not allowed. }
\item{method}{ PlusMinus algorithm used }
\item{n_cores}{ number of cores }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{This function should not be called directly, but through the generic function \code{plusminusFit} with the argument \code{validation = 'loo'}. 
}
\value{
Provides the following crossvalideted results as a list for \code{plusminus} objects:
  
\item{cvError}{ leave-one-out estimate of cv error. }
\item{in.bag}{ leave-one-out samples used for model building. }
}
\author{Richard Baumgartner (\email{richard_baumgartner@merck.com}), Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{

NOTE: This function is adapted from \code{mvr} in package \pkg{pls} with extensive modifications by Nelson Lee Afanador and Thanh Tran.

}
\seealso{
\code{\link{plusminusFit}}
}
\examples{
data(plusMinusDat)
mod1 <- plusminusFit(Y ~., data = plusMinusDat, validation = "loo", n_cores = 2)
summary(mod1)

mod1$validation$cvError
mod1$validation$in.bag  
}