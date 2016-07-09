\name{print.plusminus}
\alias{print.plusminus}
\title{Print Methods for plusminus Objects}
\description{Summary and print methods for plusminus objects.}
\usage{
\method{print}{plusminus}(x, ...)
}
\arguments{
\item{x}{ an plusminus object }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
\code{print.plusminus} Is a generic function used to print plusminus objects, such as \code{print.plusminus} for \code{plusminus} objects.
}
\author{Richard Baumgartner (\email{richard_baumgartner@merck.com}), Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(plusMinusDat)
mod1 <- plusminusFit(Y ~., data = plusMinusDat, validation = "loo", n_cores = 2)
print(mod1)
}