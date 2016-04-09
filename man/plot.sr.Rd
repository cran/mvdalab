\name{plot.sr}
\alias{plot.sr}
\title{Plotting function for Selectivity Ratio.}
\description{This function provides the ability to plot an object of class \code{sr}
}
\usage{
\method{plot}{sr}(x, variables = "all", ...)
}

\arguments{
\item{x}{\code{sr} object }
\item{variables}{ the number of variables to include the graph output. }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{

\code{plot.sr} is used to generates the graph of the selectivity ratio from \code{sr} objects.

}
\value{
The output of \code{plot.sr} is a graph of the selectivity ratio for the specified observation(s).
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\examples{
data(Penta)
## Number of bootstraps set to 500 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], 
               ncomp = 2, validation = "oob", boots = 500)
sr(mod1)
plot(sr(mod1))
}