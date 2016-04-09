\name{model.matrix}
\alias{model.matrix.mvdareg}
\title{\code{model.matrix} creates a design (or model) matrix.}
\description{This function returns the \code{model.matrix} of an \code{mvdareg} object.
}
\usage{
\method{model.matrix}{mvdareg}(object, ...)
}
\arguments{
\item{object}{ an \code{mvdareg} object }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
\code{"model.matrix.mvdareg"} is used to returns the \code{model.matrix} of an \code{mvdareg} object.
}
\value{
The design matrix for a \code{PLS} model with the specified formula and data.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
model.matrix(mod1)

}