\name{scoresplot}
\alias{scoresplot}
\title{2D Graph of the scores}
\description{Generates a 2-dimensional graph of the scores for both \code{mvdareg} and \code{mvdapca} objects.}
\usage{scoresplot(object, comps = c(1, 2), alphas = c(.95, .99),
           segments = 51, verbose = FALSE)}
\arguments{
\item{object}{ an object of class \code{mvdareg}, i.e. \code{plsFit}. }
\item{comps}{ a vector or length 2 corresponding to the number of components to include. }
\item{alphas}{ draw elliptical contours at these confidence levels. }
\item{segments}{ number of line-segments used to draw ellipse. }
\item{verbose}{ output results as a data frame}
}
\details{
\code{scoresplot} is used to extract a 2D graphical summary of the scores of PLS and PCA models.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "oob", boots = 300)
scoresplot(mod1, comp = c(1, 2))
}
