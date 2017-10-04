\name{pca.nipals}
\alias{pca.nipals}
\alias{print.npca}
\title{PCA with the NIPALS algorithm}
\description{Implements the Nonlinear Iterative Partial Least Squares (NIPALS) algorithm for computing PCA scores and loadings and intermediate steps to convergence.}
\usage{
pca.nipals(data, ncomps = 1, Iters = 500, start.vec = NULL, tol = 1e-08)
}
\arguments{
\item{data}{ A dataframe }
\item{ncomps}{ the number of components to include in the analysis. }
\item{Iters}{ Number of iterations }
\item{start.vec}{option for choosing your own starting vector}
\item{tol}{tolernace for convergence}
}
\details{
The NIPALS algorithm is a popular algorithm in multivariate data analysi for computing PCA scores and loadings. This function is specifically designed to help explore the subspace prior to convergence.  Currently only mean-centering is employed.
}
\value{

\item{Loadings}{ Loadings obtained via NIPALS }
\item{Scores}{ Scores obtained via NIPALS }
\item{Loading.Space}{ A list containing the intermediate step to convergence for the loadings }
\item{Score.Space}{ A list containing the intermediate step to convergence for the scores }

}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{

There are many good references for the NIPALS algorithm:

Risvik, Henning. "Principal component analysis (PCA) & NIPALS algorithm." (2007).

Wold, Svante, Kim Esbensen, and Paul Geladi. "Principal component analysis." Chemometrics and intelligent laboratory systems 2.1-3 (1987): 37:52.



}
\examples{
my.nipals <- pca.nipals(iris[, 1:4], ncomps = 4, tol = 1e-08)
names(my.nipals)

#Check results
my.nipals$Loadings
svd(scale(iris[, 1:4], scale = FALSE))$v

nipals.scores <- data.frame(my.nipals$Scores)
names(nipals.scores) <- paste("np", 1:4)
svd.scores <- data.frame(svd(scale(iris[, 1:4], scale = FALSE))$u)
names(svd.scores) <- paste("svd", 1:4)
Scores. <- cbind(nipals.scores, svd.scores)
plot(Scores.)

my.nipals$Loading.Space
my.nipals$Score.Space
}