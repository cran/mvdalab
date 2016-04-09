\name{coefficientsplot2D}
\alias{coefficientsplot2D}
\title{2-Dimensionsl Graphical Summary Information Pertaining to the Coefficients of a PLS}
\description{Functions to extract 2D graphical coefficients information from \code{mvdalab} objects.}
\usage{
coefficientsplot2D(object, comps = c(1, 2))
}
\arguments{
\item{object}{ an \code{mvdareg} object. }
\item{comps}{ a vector of length 2 corresponding to the number of components to include. }
}
\details{
\code{coefficientsplot2D} is used to extract a graphical summary of the coefficients of a PLS model.
If \code{comp} is \code{missing} (or is \code{NULL}), a graphical summary for the 1st and 2nd components is returned.  
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\seealso{\code{\link{loadingsplot2D}}, \code{\link{weightsplot2D}}}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
coefficientsplot2D(mod1, comp = c(1, 2))
}