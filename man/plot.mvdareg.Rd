\name{plot.mvdareg}
\alias{plot.mvdareg}
\title{General plotting function for \code{mvdareg} and \code{mvdapaca} objects.}
\description{A general plotting function for a \code{mvdareg} and \code{mvdapca} objects.}
\usage{
\method{plot}{mvdareg}(x, plottype = c("PE", "scoresplot", "loadingsplot",
                    "loadingsplot2D", "T2", "Xresids", "coefsplot", "ap.plot",
                    "weightsplot", "weightsplot2D", "acfplot"), ...)
}
\arguments{
\item{x}{ an object of class \code{"mvdareg"}, i.e., a fitted model. }
\item{plottype}{ the desired plot from an object of class \code{"mvdareg"} }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
The following plotting functions are supported:

\code{PE}, \code{scoreplot}, \code{loadingsplot}, \code{loadingsplot2D}, \code{T2}, \code{Xresids}, \code{coefsplot}, \code{ap.plot}, \code{weightsplot}, \code{weightsplot2D}, \code{acfplot}
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}

\examples{
data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "loo")
plot(mod1, plottype = "scoresplot")
\dontrun{
plot(mod1, plottype = "loadingsplot2D")
plot(mod1, plottype = "T2", ncomp = 2, phase = 1, conf = c(.95, .99))
}
}
