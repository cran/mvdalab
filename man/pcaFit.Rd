\name{pcaFit}
\alias{pcaFit}
\alias{print.mvdapca}
\alias{plot.mvdapca}
\title{Principal Component Analysis}
\description{Function to perform principal component analysis.}
\usage{pcaFit(data, scale = TRUE, ncomp = NULL)}
\arguments{
\item{data}{ an data frame containing the variables in the model. }
\item{scale}{ should scaling to unit variance be used. }
\item{ncomp}{ the number of components to include in the model (see below). }
}
\details{
The calculation is done via singular value decomposition of the data matrix.  Dummy variables are automatically created for categorical variables.
}
\value{

\code{pcaFit} returns a list containing the following components:

\item{loadings}{ X loadings }
\item{scores}{ X scores }
\item{D}{ eigenvalues }
\item{Xdata}{ X matrix }
\item{Percent.Explained}{ Explained variation in X }
\item{PRESS}{ Prediction Error Sum-of-Squares }
\item{ncomp}{ number of latent variables }
\item{method}{ PLS algorithm used }
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{

Everitt, Brian S. (2005). An R and S-Plus Companion to Multivariate Analysis. Springer-Verlag.

Edoardo Saccentia, Jos? Camacho, (2015) On the use of the observation-wise k-fold operation in PCA cross-validation, J. Chemometrics 2015; 29: 467-478.


}
\seealso{\code{\link{loadingsplot2D}}, \code{\link{T2}}, \code{\link{Xresids}}, \code{\link{ScoreContrib}}}
\examples{
data(iris)
pc1 <- pcaFit(iris, scale = TRUE, ncomp = NULL)
pc1

print(pc1) #Model summary
plot(pc1) #MSEP
PE(pc1) #X-explained variance

T2(pc1, ncomp = 2) #T2 plot

Xresids(pc1, ncomp = 2) #X-residuals plot

scoresplot(pc1) #scoresplot variable importance

(SC <- ScoreContrib(pc1, obs1 = 1:9, obs2 = 10:11))  #score contribution
plot(SC)  #score contribution plot

loadingsplot(pc1, ncomp = 1) #loadings plot
loadingsplot(pc1, ncomp = 1:2) #loadings plot
loadingsplot(pc1, ncomp = 1:3) #loadings plot
loadingsplot(pc1, ncomp = 1:7) #loadings plot
loadingsplot2D(pc1, comps = c(1, 2)) #2-D loadings plot
loadingsplot2D(pc1, comps = c(2, 3)) #2-D loadings plot
}
