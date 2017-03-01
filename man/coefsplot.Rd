\name{coefsplot}
\alias{coefsplot}
\title{Graphical Summary Information Pertaining to the Regression Coefficients}
\description{Functions to extract regression coefficient bootstrap information from mvdalab objects.}
\usage{coefsplot(object, ncomp = object$ncomp, conf = 0.95, verbose = FALSE)}
\arguments{
\item{object}{ an mvdareg object.  A fitted model. }
\item{ncomp}{ the number of components to include. }
\item{conf}{ for a bootstrapped model, the confidence level to use. }
\item{verbose}{ output results as a data frame}
}
\details{

\code{coefficients} is used to extract a graphical summary of the regression coefficients of a PLS model.

If \code{comps} is \code{missing} (or is \code{NULL}), a graphical summary for the nth component regression estimates are returned.  Otherwise, if \code{comps} is given parameters for a model with only the requested component \code{comps} is returned.

Bootstrap graphcal summaries provided are when \code{method = oob}.

}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "oob", boots = 500)
coefsplot(mod1, ncomp = 1:2)
}
