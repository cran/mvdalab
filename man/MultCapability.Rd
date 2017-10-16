\name{MultCapability}
\alias{MultCapability}
\title{
Principal Component Based Multivariate Process Capability Indices
}
\description{
Provides three multivariate capability indices for correlated multivariate processes based on Principal Component Analysis.
}
\usage{MultCapability(data, lsls, usls, targets, ncomps = NULL, Target = FALSE)
}

\arguments{
\item{data}{ a multivariable dataset }
\item{lsls}{ is the vector of the lower specification limits }  
\item{usls}{ is the vector of the upper specification limits }
\item{targets}{ is the vector of the target of the process }
\item{ncomps}{ is the number of principal component to use }
\item{Target}{ Use \code{targets} for calculation of univariate PpKs; otherwise the average is used }
}
\details{

\code{ncomps} has to be set prior to running the analysis.  The user is strongly encouraged to use \code{pcaFit} in order to determine the optimal number of principal components using cross-validation.

When the parameter targets is not specified, then is estimated of centered way as targets = lsls + (usls - lsls)/2. 

Ppk values are provided to allow the user to compare the multivariate results to the univariate results.

}
\value{

A list with the following elements:

For \code{mpca_wang}, the following is returned:
\item{ncomps}{ number of components used }
\item{mcp_wang}{ index greater than 1, the process is capable }
\item{mcpk_wang}{ index greater than 1, the process is capable }
\item{mcpm_wang}{ index greater than 1, the process is capable }
\item{mcpmk_wang}{ index greater than 1, the process is capable }

For \code{mcp_xe}, the following is returned:
\item{ncomps}{ number of components used }
\item{mcp_wang_2}{ index greater than 1, the process is capable }
\item{mcpk_wang_2}{ index greater than 1, the process is capable }
\item{mcpm_wang_2}{ index greater than 1, the process is capable }
\item{mcpmk_wang_2}{ index greater than 1, the process is capable }

For \code{mpca_wang_2}, the following is returned:
\item{ncomps}{ number of components used }
\item{mcp_xe}{ index greater than 1, the process is capable }
\item{mcpk_xe}{ index greater than 1, the process is capable }
\item{mcpm_xe}{ index greater than 1, the process is capable }
\item{mcpmk_xe}{ index greater than 1, the process is capable }

For Ppk, the following is returned:
\item{Individual.Ppks}{ univariate Ppks; index greater than 1, the process is capable }

}
\author{Nelson Lee Afanador (\email{nelson.afanador@mvdalab.com})}
\references{

Wang F, Chen J (1998). Capability index using principal components analysis. Quality Engineering, 11, 21-27.

Xekalaki E, Perakis M (2002). The Use of principal component analysis in the assessment of process capability indices.
Proceedings of the Joint Statistical Meetings of the American Statistical Association, The Institute of Mathematical Statistics, The Canadian Statistical Society. New York.

Wang, C (2005). Constructing multivariate process capability indices for short-run production. The International Journal of Advanced Manufacturing Technology, 26, 1306-1311.

Scagliarini, M (2011). Multivariate process capability using principal component analysis in the presence of measurement errors. AStA Adv Stat Anal, 95, 113-128.

Santos-Fernandez E, Scagliarini M (2012). "MPCI: An R Package for Computing Multivariate Process Capability Indices". Journal of Statistical Software, 47(7), 1-15, URL http://www.jstatsoft.org/v47/i07/.

}

\examples{
data(Wang_Chen_Sim)
lsls1 <- c(2.1, 304.5, 304.5)
usLs1 <- c(2.3, 305.1, 305.1)
targets1 <- c(2.2, 304.8, 304.8)

MultCapability(Wang_Chen_Sim, lsls = lsls1, usls = usLs1, targets = targets1, ncomps = 2)

data(Wang_Chen)
targets2 <- c(177, 53)
lsls2 <- c(112.7, 32.7)
usLs2 <- c(241.3, 73.3)

MultCapability(Wang_Chen, lsls = lsls2, usls = usLs2, targets = targets2, ncomps = 1)

}

