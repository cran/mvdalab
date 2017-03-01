\name{ScoreContrib}
\alias{ScoreContrib}
\title{Generates a score contribution plot}
\description{Generates a the Score Contribution Graph both \code{mvdareg} and \code{mvdapca} objects.}
\usage{ScoreContrib(object, ncomp = 1:object$ncomp, obs1 = 1, obs2 = NULL)}
\arguments{
\item{object}{ an object of class \code{mvdareg} or \code{mvdapca}. }
\item{ncomp}{ the number of components to include in the model (see below). }
\item{obs1}{ the first observaion(s) in the score(s) comparison. }
\item{obs2}{ the second observaion(s) in the score(s) comparison. }

}
\details{
\code{ScoreContrib} is used to generates the score contributions for both PLS and PCA models.  Up to two groups of score(s) can be selected.  If only one group is selected, the contribution is measured to the model average.  For PLS models the PCA loadings are replaced with the PLS weights.
}
\value{
The output of \code{ScoreContrib} is a matrix of score contributions for the specified observation(s).
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{
MacGregor, Process Monitoring and Diagnosis by Multiblock PLS Methods, May 1994 Vol. 40, No. 5 AIChE Journal.
}
\examples{
data(Penta)
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "none")
Score.Contributions1 <- ScoreContrib(mod1, ncomp = 1:2, obs1 = 1, obs2 = 3)
plot(Score.Contributions1, ncomp = 2)

\dontrun{
data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod2 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               ncomp = 2, validation = "none")
Score.Contributions2 <- ScoreContrib(mod2, obs1 = 1, obs2 = 3)
plot(Score.Contributions2)
Score.Contributions3 <- ScoreContrib(mod1, obs1 = c(1, 3), obs2 = c(5:10))
plot(Score.Contributions3)
}

###  PLS MODEL FIT WITH method = 'wrtpls' and validation = 'none', i.e. WRT-PLS is performed ###
\dontrun{
mod3 <- plsFit(Sepal.Length ~., scale = TRUE, data = iris,
               method = "wrtpls", validation = "none") #ncomp is ignored
Score.Contributions4 <- ScoreContrib(mod3, ncomp = 1:5, obs1 = 1, obs2 = 3)
plot(Score.Contributions4, ncomp = 5)
}

#PCA Model
pc1 <- pcaFit(Penta[, -1], ncomp = 2)
Score.Contributions1 <- ScoreContrib(pc1, obs1 = 1, obs2 = 3)
plot(Score.Contributions1)
}
