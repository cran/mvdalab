\name{plot.cp}
\alias{plot.cp}
\title{Plotting Function for Score Contributions.}
\description{This function generates a plot an object of class \code{score.contribution}
}
\usage{
\method{plot}{cp}(x, ncomp = "Overall", ...)
}

\arguments{
\item{x}{ \code{score.contribution} object }
\item{ncomp}{ the number of components to include the graph output. }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{
A graph of the score contributions for \code{ScoreContrib} objects.
}
\value{
The output of \code{plot} is a graph of score contributions for the specified observation(s).
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
Score.Contributions1 <- ScoreContrib(mod1, obs1 = 1, obs2 = 3)
plot(Score.Contributions1, ncomp = 1)

#PCA Model
pc1 <- pcaFit(Penta[, -1])
Score.Contributions1 <- ScoreContrib(mod1, obs1 = 1, obs2 = 3)
plot(Score.Contributions1, ncomp = 1)
}