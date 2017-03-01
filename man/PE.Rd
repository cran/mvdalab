\name{PE}
\alias{PE}
\title{Percent Explained Variation of X}
\description{This function provides both the cumulative and individual percent explained for the X-block for an \code{mvdareg} and \code{mvdapca} objects.}
\usage{PE(object, verbose = FALSE)}
\arguments{
\item{object}{ an object of class \code{mvdareg} or \code{mvdapca} objects. }
\item{verbose}{ output results as a data frame}
}
\details{
This function provides both the cumulative and individual percent explained for the X-block for an \code{mvdareg} or \code{mvdapca} objects.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{

mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "none")
PE(mod1)

\dontrun{
data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod2 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "oob", boots = 300)
PE(mod2)
}

}
