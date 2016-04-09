\name{PE}
\alias{PE}
\title{Percent Explained Variation of X}
\description{This function provides both the cumulative and individual percent explained for the X-block for an \code{mvdareg} and \code{mvdapca} objects.}
\usage{PE(object)}
\arguments{
\item{object}{ an object of class \code{mvdareg} or \code{mvdapca} objects. }
}
\details{
This function provides both the cumulative and individual percent explained for the X-block for an \code{mvdareg} or \code{mvdapca} objects.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
PE(mod1)
}