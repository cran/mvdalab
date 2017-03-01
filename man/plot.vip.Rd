\name{plot.vip}
\alias{plot.vip}
\title{Plotting function for Variable Importance in the Projection}
\description{This function generates a plot an object of class \code{vip}.
}
\usage{
\method{plot}{vip}(x, ncomp = 1, ...)
}

\arguments{
\item{x}{\code{vip} object }
\item{ncomp}{ the number of components to include the graph output. }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{

\code{plot.vip} is used to generates the graph of the variable in the projection from \code{vip} objects.


}
\value{
The output of \code{plot.vip} is a graph of the variable importance in the projection.
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}

\examples{
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, method = "bidiagpls", validation = "loo")
VIP1 <- vip(mod1, conf = .95)
plot(VIP1, ncomp = 2)

\dontrun{
data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod2 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "oob", boots = 300)
VIP2 <- vip(mod2, conf = .95)
plot(VIP2)
}

###  PLS MODEL FIT WITH method = 'wrtpls' and validation = 'none', i.e. WRT-PLS is performed ###
\dontrun{
mod3 <- plsFit(Sepal.Length ~., scale = TRUE, data = iris,
               method = "wrtpls", validation = "none") #ncomp is ignored
plot(vip(mod3), ncomp = 1:2)
}
}
