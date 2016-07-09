\name{plusminusFit}
\alias{plusminusFit}
\alias{summary.plusminus}
\alias{summary.plusminus.default}
\title{Plus-Minus (Mas-o-Menos) Classifier}
\description{Functions to perform plus-minus classifier with a formula interface. Leave one out crossvalidation also implemented. Model extraction, plot, print and summary methods are also implemented.}
\usage{
plusminusFit(formula, data, subset, na.action, method = "plusminus", n_cores = 2, 
                         validation = c("loo", "none"), model = TRUE, 
                         x = FALSE, y = FALSE, ...)
  
\method{summary}{plusminus}(object,...)
}
\arguments{
\item{formula}{ a model formula (see below). }
\item{data}{ an optional data frame containing the variables in the model. }
\item{subset}{ an optional vector specifying a subset of observations to be used in the fitting process. }
\item{na.action}{ a function which indicates what should happen when the data contain \code{NAs}. The default is set by the \code{na.action} setting of options, and is \code{na.fail} if that is unset. The default is \code{na.omit}. Another possible value is \code{NULL}, no action. Value \code{na.exclude} can be useful. }
\item{method}{ the classification algorithm to be used. }
\item{n_cores}{ Number of cores to run for parallel processing.  Currently set to 2 with the max being 4. }
\item{validation}{ character. What kind of (internal) validation to use. See below. }
\item{model}{ an optional data frame containing the variables in the model. }
\item{x}{ a logical. If TRUE, the model matrix is returned. }
\item{y}{ a logical. If TRUE, the response is returned. }
\item{object}{ an object of class \code{"plusminus"}, i.e., a fitted model. }
\item{\dots}{ additional arguments, passed to the underlying fit functions, and \code{plusminus}. Currently not in use.}
}
\details{The function fits a Plus-Minus classifier.

The formula argument should be a symbolic formula of the form response ~ terms, where response is the name of the response vector and terms is the name of one or more predictor matrices, usually separated by +, e.g., y ~ X + Z. See \code{\link{lm}} for a detailed description. The named variables should exist in the supplied data data frame or in the global environment. The chapter Statistical models in R of the manual An Introduction to R distributed with R is a good reference on formulas in R.


If \code{validation = "loo"}, leave-one-out cross-validation is performed. If \code{validation = "none"}, no cross-validation is performed.
}
\value{
An object of class \code{plusminus} is returned. The object contains all components returned by the underlying fit function. In addition, it contains the following:
\item{coefficients}{Plus-Minus regression coefficients }
\item{X}{ X matrix }
\item{Y}{ actual response values (class labels)}
\item{val.method}{ validation method }
\item{call}{ model call }
\item{terms}{ model terms }
\item{mm}{model matrix}
\item{model}{ fitted model }
}
\author{Richard Baumgartner (\email{richard_baumgartner@merck.com}), Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{
Zhao et al.: Mas-o-menos: a simple sign averaging method for discriminationin genomic data analysis. Bioinformatics, 30(21):3062-3069,2014.
}
\seealso{\code{\link{plusminus.fit}}, \code{\link{plusminus.loo}}}
\examples{
###  Plus-Minus CLASSIFIER WITH validation = 'loo', i.e. leave-one-out CV ###
data(plusMinusDat)
mod1 <- plusminusFit(Y ~., data = plusMinusDat, validation = "loo", n_cores = 2)
summary(mod1)

###  PLUS-Minus CLASSIFIER WITH validation = 'none', i.e. no CV ###

mod2 <- plusminusFit(Y ~., data = plusMinusDat, validation = "none", n_cores = 2)
summary(mod2)
}