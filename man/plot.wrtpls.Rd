\name{plot.wrtpls}
\alias{plot.wrtpls}
\title{Plots of the Output of a Permutation Distribution for an \code{mvdareg} Object with \code{method = "bidiagpls"}}
\description{This takes an \code{mvdareg} object fitted with \code{method = "bidiagpls"} and produces a graph of the bootstrap distribution and its corresponding normal quantile plot for a  variable of interest.}
\usage{
\method{plot}{wrtpls}(x, comp = 1:object$ncomp, distribution = "log", ...)
}
\arguments{
\item{x}{ an object of class \code{"mvdareg"}, i.e., a \code{plsFit}. }
\item{comp}{ number of latent variables to generate the permutation distribution }
\item{distribution}{ plot the \code{"log"}, or \code{"actual"}, of the permutation distribution }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
The function generates the permutation distribution and normal quantile plot for a \code{mvdareg} model when \code{method = "bidiagpls"} is specified.
}
\value{
The output of \code{plot.wrtpls} is a histogram of the permutation distribution with the following vertical line indicators.

Solid line = Actual Value;
Dashed Line = Critical Value from t-distribution at the model specifed alpha;
Dotted line = Quantile at the model specifed alpha

}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\seealso{\code{\link{bca.cis}}}
\examples{
data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               method = "wrtpls", validation = "none")
## Not run
## plot.wrtpls(mod1, distribution = "log")
}
