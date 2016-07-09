\name{plusminus.fit}
\alias{plusminus.fit}
\title{PlusMinus (Mas-o-Menos)}
\description{Plus-Minus classifier}
\usage{plusminus.fit(XX, YY, ...)}
\arguments{
\item{XX}{ a matrix of observations. \code{NAs} and \code{Infs} are not allowed. }
\item{YY}{ a vector. \code{NAs} and \code{Infs} are not allowed. }
\item{\dots}{ additional arguments.  Currently ignored. }
}
\details{This function should not be called directly, but through \code{plusminusFit} with the argument \code{method="plusminus"}. It implements the Plus-Minus algorithm.  
}
\value{
An object of class \code{plusminus} is returned. The object contains all components returned by the underlying fit function. In addition, it contains the following:
\item{coefficients}{ regression coefficients }
\item{Y}{ response values }
\item{X}{ scaled predictors}
}
\author{Richard Baumgartner (\email{richard_baumgartner@merck.com}), Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{
Zhao et al. (2014) Mas-o-menos: a simple sign averaging method for discriminationin genomic data analysis. Bioinformatics, 30(21):3062-3069.
}
\seealso{\code{\link{plusminusFit}}}