\name{contr.niets}
\alias{contr.niets}
\title{Cell Means Contrast Matrix }
\description{This function generates a cell means contrast matrix to support PLS models.}
\usage{contr.niets(n, contrasts)}
\arguments{
\item{n}{ A vector of levels for a factor, or the number of levels. }
\item{contrasts}{ a logical indicating whether contrasts should be computed; set to \code{FALSE} 
  in order to generate required contrast matrix.}
}
\details{
This function uses \code{contr.treatment} to generate a cell means contrast matrix in support of PLS models. 
}
\value{
For datasets with categorical variables it produces the needed design matrix.
}
\author{Nelson Lee Afanador}
\examples{
# Three levels
levels <- LETTERS[1:3]
contr.niets(levels)

# Two levels
levels <- LETTERS[1:2]
contr.niets(levels)
}