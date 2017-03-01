\name{plot.plusminus}
\alias{plot.plusminus}
\title{2D Graph of the PCA scores associated with a plusminusFit}
\description{Generates a 2-dimensional graph of the scores for both \code{plusminus} objects.}
\usage{
\method{plot}{plusminus}(x, ncomp = 2, comps = c(1, 2), ...)
}
\arguments{
\item{x}{ an object of class \code{plusminus}, i.e. \code{plusminusFit}. }
\item{ncomp}{ the number of components to include in the model (see below). }
\item{comps}{ a vector or length 2 corresponding to the number of components to include. }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
\code{plot.plusminus} is used to extract a 2D graphical summary of the PCA scores associated with a \code{plusminus} object.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
###  PLUS-Minus CLASSIFIER WITH validation = 'none', i.e. no CV ###
data(plusMinusDat)
mod1 <- plusminusFit(Y ~., data = plusMinusDat, validation = "none", n_cores = 2)
plot(mod1, ncomp = 2, comps = c(1, 2))

###  Plus-Minus CLASSIFIER WITH validation = 'loo', i.e. leave-one-out CV ###
\dontrun{
data(plusMinusDat)
mod2 <- plusminusFit(Y ~., data = plusMinusDat, validation = "loo", n_cores = 2)
plot(mod2, ncomp = 2, comps = c(1, 2))
}
}
