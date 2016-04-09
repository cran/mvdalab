\name{print.mvdalab}
\alias{print.mvdareg}
\title{Print Methods for mvdalab Objects}
\description{Summary and print methods for mvdalab objects.}
\usage{
\method{print}{mvdareg}(x, ...)
}
\arguments{
\item{x}{ an mvdalab object }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
\code{print.mvdalab} Is a generic function used to print mvdalab objects, such as \code{print.empca} for \code{imputeEM}, \code{print.mvdapca} for \code{mvdapca} objects, and \code{summary.mvdareg} for \code{mvdareg} objects.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
print(mod1, ncomp = 2)
summary(mod1, ncomp = 2)
}