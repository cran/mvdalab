\name{boot.plots}
\alias{boot.plots}
\title{Plots of the Output of a Bootstrap Simulation for an \code{mvdared} Object}
\description{This takes an \code{mvdareg} object fitted with \code{validation = "oob"} and produces a graph of the bootstrap distribution and its corresponding normal quantile plot for a  variable of interest.}
\usage{
boot.plots(object, comp = object$ncomp, parm = NULL, 
           type = c("coefs", "weights", "loadings"))
}
\arguments{
\item{object}{ an object of class \code{"mvdareg"}, i.e., a \code{plsFit}. }
\item{comp}{ latent variable from which to generate the bootstrap distribution for a specific parameter }
\item{parm}{ a parameter for which to generate the bootstrap distribution }
\item{type}{ input parameter vector  }
}
\details{
The function fits computes the bootstrap distribution and normal quantile plot for a bootstrapped \code{mvdareg} model given \code{validation = "oob"} for \code{type = c("coefs", "weights", "loadings")}.  If \code{parm = NULL} a paramater is chosen at random.
}
\value{
The output of \code{boot.plots} is a histogram of the bootstrap distribution and the corresponding normal quantile plot.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\seealso{\code{\link{bca.cis}}}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
boot.plots(mod1, type = "coefs", parm = NULL)
}