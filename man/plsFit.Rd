\name{plsFit}
\alias{plsFit}
\alias{mvdareg}
\alias{summary.mvdareg}
\alias{summary.mvdareg.default}
\title{Partial Least Squares Regression}
\description{Functions to perform partial least squares regression with a formula interface. Bootstraping can be used. Prediction, residuals, model extraction, plot, print and summary methods are also implemented.}
\usage{
plsFit(formula, data, subset, ncomp = NULL, na.action, 
method = c("bidiagpls", "wrtpls"), scale = TRUE, n_cores = 2, 
alpha = 0.05, perms = 2000, validation = c("none", "oob", "loo"), 
boots = 1000, model = TRUE, parallel = FALSE,
x = FALSE, y = FALSE, ...) 
\method{summary}{mvdareg}(object, ncomp = object$ncomp, digits = 3, ...)
}
\arguments{
\item{formula}{ a model formula (see below). }
\item{data}{ an optional data frame containing the variables in the model. }
\item{subset}{ an optional vector specifying a subset of observations to be used in the fitting process. }
\item{ncomp}{ the number of components to include in the model (see below). }
\item{na.action}{ a function which indicates what should happen when the data contain \code{NAs}. The default is set by the \code{na.action} setting of options, and is \code{na.fail} if that is unset. The default is \code{na.omit}. Another possible value is \code{NULL}, no action. Value \code{na.exclude} can be useful. }
\item{method}{ the multivariate regression algorithm to be used. }
\item{scale}{ should scaling to unit variance be used. }
\item{n_cores}{ Number of cores to run for parallel processing.  Currently set to 2 with the max being 4. }
\item{alpha}{ the significance level for \code{wrtpls} }
\item{perms}{ the number of permutations to run for \code{wrtpls} }
\item{validation}{ character. What kind of (internal) validation to use. See below. }
\item{boots}{Number of bootstrap samples when \code{validation = 'oob'}}
\item{model}{ an optional data frame containing the variables in the model. }
\item{parallel}{ should parallelization be used. }
\item{x}{ a logical. If TRUE, the model matrix is returned. }
\item{y}{ a logical. If TRUE, the response is returned. }
\item{object}{ an object of class \code{"mvdareg"}, i.e., a fitted model. }
\item{digits}{the number of decimal place to output with \code{summary.mvdareg}}
\item{\dots}{ additional arguments, passed to the underlying fit functions, and \code{mvdareg}. Currently not in use.}
}
\details{The function fits a partial least squares (PLS) model with 1, ..., \code{ncomp} number of latent variables. Multi-response models are not supported.

The type of model to fit is specified with the method argument. Currently two PLS algorithms are available: the bigiag2 algorithm ("bigiagpls" and "wrtpls").

The formula argument should be a symbolic formula of the form response ~ terms, where response is the name of the response vector and terms is the name of one or more predictor matrices, usually separated by +, e.g., y ~ X + Z. See \code{\link{lm}} for a detailed description. The named variables should exist in the supplied data data frame or in the global environment. The chapter Statistical models in R of the manual An Introduction to R distributed with R is a good reference on formulas in R.

The number of components to fit is specified with the argument \code{ncomp}. It this is not supplied, the maximal number of components is used.

Note that if the number of samples is <= 15, oob validation may fail.  It is recommended that you PLS with \code{validation = "loo"}.

If \code{method = "bidiagpls"} and \code{validation = "oob"}, bootstrap cross-validation is performed. Bootstrap confidence intervals are provided for \code{\link{coefficients}}, \code{weights}, \code{loadings}, and \code{y.loadings}.  The number of bootstrap samples is specified with the argument \code{boots}. See \code{mvdaboot} for details.

If \code{method = "bidiagpls"} and \code{validation = "loo"}, leave-one-out cross-validation is performed.

If \code{method = "bidiagpls"} and \code{validation = "none"}, no cross-validation is performed.  Note that the number of components, \code{ncomp}, is set to \code{min(nobj - 1, npred)}

If \code{method = "wrtpls"} and \code{validation = "none"}, The Weight Randomization Test for the selection of the number of components is performed.  Note that the number of components, \code{ncomp}, is set to \code{min(nobj - 1, npred)}


}
\value{
An object of class \code{mvdareg} is returned. The object contains all components returned by the underlying fit function. In addition, it contains the following:
\item{loadings}{ X loadings }
\item{weights}{ weights }
\item{D2.values}{ bidiag2 matrix }
\item{iD2}{ inverse of bidiag2 matrix }
\item{Ymean}{ mean of reponse variable }
\item{Xmeans}{ mean of predictor variables }
\item{coefficients}{ PLS regression coefficients }
\item{y.loadings}{ y-loadings }
\item{scores}{ X scores }
\item{R}{ orthogonal weights }
\item{Y.values}{ scaled response values }
\item{Yactual}{ actual response values }
\item{fitted}{ fitted values }
\item{residuals}{ residuals }
\item{Xdata}{ X matrix }
\item{iPreds}{ predicted values }
\item{y.loadings2}{ scaled y-loadings }
\item{ncomp}{ number of latent variables }
\item{method}{ PLS algorithm used }
\item{scale}{ scaling used }
\item{validation}{ validation method }
\item{call}{ model call }
\item{terms}{ model terms }
\item{model}{ fitted model }
}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com}), Thanh Tran (\email{thanh.tran@mvdalab.com})}
\references{
NOTE: This function is adapted from \code{mvr} in package \pkg{pls} with extensive modifications by Nelson Lee Afanador and Thanh Tran.
}
\seealso{\code{\link{bidiagpls.fit}}, \code{\link{mvdaboot}}, \code{\link{boot.plots}},
\code{\link{R2s}}, \code{\link{PE}}, \code{\link{ap.plot}},
\code{\link{T2}}, \code{\link{Xresids}}, \code{\link{smc}},
\code{\link{scoresplot}}, \code{\link{ScoreContrib}}, \code{\link{sr}},
\code{\link{loadingsplot}}, \code{\link{weightsplot}}, \code{\link{coefsplot}},
\code{\link{coefficientsplot2D}}, \code{\link{loadingsplot2D}},
\code{\link{weightsplot2D}}, 
\code{\link{bca.cis}}, \code{\link{coefficients.boots}}, \code{\link{loadings.boots}},
\code{\link{weight.boots}}, \code{\link{coefficients}}, \code{\link{loadings}},
\code{\link{weights}}, \code{\link{BiPlot}}, \code{\link{jk.after.boot}}}
\examples{
###  PLS MODEL FIT WITH method = 'bidiagpls' and validation = 'oob', i.e. bootstrapping ###
data(Penta)
## Number of bootstraps set to 300 to demonstrate flexibility
## Use a minimum of 1000 (default) for results that support bootstraping
mod1 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], method = "bidiagpls",
               ncomp = 2, validation = "oob", boots = 300)
summary(mod1) #Model summary

###  PLS MODEL FIT WITH method = 'bidiagpls' and validation = 'loo', i.e. leave-one-out CV ###
\dontrun{
mod2 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], method = "bidiagpls",
               ncomp = 2, validation = "loo")
summary(mod2) #Model summary
}

###  PLS MODEL FIT WITH method = 'bidiagpls' and validation = 'none', i.e. no CV is performed ###
\dontrun{
mod3 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1], method = "bidiagpls",
               ncomp = 2, validation = "none")
summary(mod3) #Model summary
}
###  PLS MODEL FIT WITH method = 'wrtpls' and validation = 'none', i.e. WRT-PLS is performed ###
\dontrun{
mod4 <- plsFit(log.RAI ~., scale = TRUE, data = Penta[, -1],
               method = "wrtpls", validation = "none")
summary(mod4) #Model summary
plot.wrtpls(mod4)
}
}
