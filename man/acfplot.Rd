\name{acfplot}
\alias{acfplot}
\title{Plot of Auto-correlation Funcion}
\description{This function computes the autocorrelation function estimates for a selected parameter.}
\usage{acfplot(object, parm = NULL)}
\arguments{
\item{object}{ an object of class \code{mvdareg}, i.e., \code{plsFit}. }
\item{parm}{ a chosen predictor variable; if \code{NULL} a random predictor variable is chosen}
}
\details{
This function computes the autocorrelation function estimates for a selected parameter, via \code{acf}, and generates a graph that allows the analyst to assess the need for an autocorrelation adjustment in the \code{\link{smc}}.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{
This function is built using the \code{acf} function in the \pkg{stats} R package.

Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth Edition. Springer-Verlag.
}
\seealso{\code{\link{smc}}, \code{\link{smc.acfTest}}}
\examples{
data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "oob", boots = 300)
acfplot(mod1, parm = NULL)
}
