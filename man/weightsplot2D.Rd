\name{weightsplot2D}
\alias{weightsplot2D}
\title{Extract a 2-Dimensional Graphical Summary Information Pertaining to the weights of a PLS Analysis}
\description{Functions to extract 2D graphical weights information from \code{mvdalab} objects.}
\usage{weightsplot2D(object, comps = c(1, 2), verbose = FALSE)}
\arguments{
\item{object}{ an mvdareg object, i.e. \code{plsFit}. }
\item{comps}{ a vector or length 2 corresponding to the number of components to include.} \item{verbose}{ output results as a data frame}
}
\details{

\code{weightsplot2D} is used to extract a graphical summary of the weights of a PLS model.

If \code{comp} is missing (or is \code{NULL}), a graphical summary for the 1st and 2nd componentsare returned.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "loo")
weightsplot2D(mod1, comp = c(1, 2))
}
