\name{summary}

\alias{summary}

\alias{summary.varest}

\alias{summary.svarest}

\alias{summary.svecest}

\alias{print.varsum}

\alias{print.svarsum}

\alias{print.svecsum}

\title{Summary method for objects of class varest, svarest and svecest}

\description{
  'summary' methods for class '"varest"', '"svarest"' and '"svecest"'.
}

\usage{
\method{summary}{varest}(object, equations = NULL, ...)
\method{print}{varsum}(x, digits = max(3, getOption("digits") - 3),
signif.stars = getOption("show.signif.stars"), ...)
\method{summary}{svarest}(object,  ...)
\method{print}{svarsum}(x, digits = max(3, getOption("digits") - 3), ...)
\method{summary}{svecest}(object,  ...)
\method{print}{svecsum}(x, digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{object}{Object of class \sQuote{\code{varest}}, usually, a
    result of a call to \code{VAR}, or object of class \sQuote{\code{svarest}}, usually, a
    result of a call to \code{SVAR}, or object of class \sQuote{\code{svecest}}, usually, a
    result of a call to \code{SVEC}.}
  \item{equations}{Character vector of endogenous variable names for
  which summary results should be returned. The default is \code{NULL}
  and results are returned for all equations in the VAR.}
  \item{x}{Object with class attribute \sQuote{varsum}, \sQuote{svarsum}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{signif.stars}{logical. If 'TRUE', \sQuote{significance stars}
  are printed for each coefficient.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\value{
  Returns either a list with class attribute \code{varsum} which contains the
  following elements:
  
  \item{names}{Character vector with the names of the endogenous
    correlation matrix of VAR residuals.}
  \item{logLik}{Numeric, value of log Likelihood.}
  \item{obs}{Integer, sample size.}
  \item{roots}{Vector, roots of the characteristic polynomial.}
  \item{type}{Character vector, deterministic regressors included in VAR:}
  \item{call}{Call, the initial call to \code{VAR}.}
  
  Or a list with class attribute \code{svarsum} which contains the
  following elements:
  
  \item{type}{Character, the type of SVAR-model.}
  \item{A}{Matrix, estimated coefficients for A matrix.}
  \item{B}{Matrix, estimated coefficients for B matrix.}
  \item{Ase}{Matrix, standard errors for A matrix.}
  \item{Bse}{Matrix, standard errors for B matrix.}
  \item{LRIM}{Matrix, long-run impact coefficients for \code{BQ}.}
  \item{Sigma.U}{Matrix, variance/covariance of reduced form residuals.}
  \item{logLik}{Numeric, value of log-Likelihood.}
  \item{LR}{htest, LR result of over-identification test.}
  \item{obs}{Integer, number of observations used.}
  \item{opt}{List, result of \code{optim()}.}
  \item{iter}{Integer, the count of iterations.}
  \item{call}{Call, the call to \code{SVAR()}.}
  
  Or a list with class attribute \code{svecsum} which contains the
  following elements:
  
  \item{type}{Character, the type of SVEC-model.}
  \item{SR}{Matrix, contemporaneous impact matrix.}
  \item{LR}{Matrix, long-run impact matrix.}
  \item{SRse}{Matrix, standard errors for SR matrix.}
  \item{LRse}{Matrix, standard errors for LR matrix.}
  \item{Sigma.U}{Matrix, variance/covariance of reduced form residuals.}
  \item{logLik}{Numeric, value of log-Likelihood.}
  \item{LRover}{htest, LR result of over-identification test.}
  \item{obs}{Integer, number of observations used.}
  \item{r}{Integer, co-integration rank of VECM.}
  \item{iter}{Integer, the count of iterations.}
  \item{call}{Call, the call to \code{SVEC()}.}  
}

\author{Bernhard Pfaff}

\seealso{
  \code{\link{VAR}}, \code{\link{SVAR}}, \code{\link{SVEC}}  
}

\examples{
data(Canada)
## summary-method for varest
var.2c <- VAR(Canada, p = 2 , type = "const")
summary(var.2c)
## summary-method for svarest
amat <- diag(4)
diag(amat) <- NA
amat[2, 1] <- NA
amat[4, 1] <- NA
## Estimation method scoring
svar.a <- SVAR(x = var.2c, estmethod = "scoring", Amat = amat, Bmat = NULL,
max.iter = 100, maxls = 1000, conv.crit = 1.0e-8)
summary(svar.a)
## summary-method for svecest
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",
              ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec.b <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot =
FALSE)
summary(svec.b) 
}

\keyword{regression}
