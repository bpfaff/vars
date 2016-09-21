\name{VAR}

\alias{VAR}

\alias{print.varest}

\title{Estimation of a VAR(p)}

\encoding{latin1}

\description{
  Estimation of a VAR by utilising OLS per equation.
}

\usage{
VAR(y, p = 1, type = c("const", "trend", "both", "none"),
season = NULL, exogen = NULL, lag.max = NULL,
ic = c("AIC", "HQ", "SC", "FPE"))
\method{print}{varest}(x, digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{y}{Data item containing the endogenous variables}
  \item{p}{Integer for the lag order (default is p=1).}
  \item{type}{Type of deterministic regressors to include.}
  \item{season}{Inlusion of centered seasonal dummy variables (integer
    value of frequency).}
  \item{exogen}{Inlusion of exogenous variables.}
  \item{lag.max}{Integer, determines the highest lag order for lag
    length selection according to the choosen \code{ic}.}
  \item{ic}{Character, selects the information criteria, if
    \code{lag.max} is not \code{NULL}.}
  \item{x}{Object with class attribute \sQuote{varest}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{
  Estimates a VAR by OLS per equation. The model is of the following
  form:

  \deqn{
    \bold{y}_t = A_1 \bold{y}_{t-1} + \ldots + A_p \bold{y}_{t-p} +
    CD_t + \bold{u}_t
  }

  where \eqn{\bold{y}_t} is a \eqn{K \times 1} vector of endogenous
  variables and \eqn{u_t} assigns a spherical disturbance term of the
  same dimension. The coefficient matrices \eqn{A_1, \ldots, A_p} are of
  dimension \eqn{K \times K}. In addition, either a constant and/or a
  trend can be included as deterministic regressors as well as centered
  seasonal dummy variables and/or exogenous variables (term \eqn{CD_T}, by
  setting the \code{type} argument to the corresponding value and/or
  setting \code{season} to the desired frequency (integer) and/or providing a
  matrix object for \code{exogen}, respectively. The default for \code{type} is
  \code{const} and for \code{season} and \code{exogen} the default is
  set to \code{NULL}.\cr
  If for \code{lag.max} an integer value is provided instead of
  \code{NULL} (the default), the lag length is determined by the
  selected information criteria in \code{ic}, the default is Akaike.
}

\value{
  A list with class attribute \sQuote{\code{varest}} holding the
  following elements:\cr

  \item{varresult}{list of \sQuote{\code{lm}} objects.}
  \item{datamat}{The data matrix of the endogenous and explanatory variables.}
  \item{y}{The data matrix of the endogenous variables}
  \item{type}{A character, specifying the deterministic regressors.}
  \item{p}{An integer specifying the lag order.}
  \item{K}{An integer specifying the dimension of the VAR.}
  \item{obs}{An integer specifying the number of used observations.}
  \item{totobs}{An integer specifying the total number of observations.}
  \item{restrictions}{Either \code{NULL} or a matrix object containing
    the zero restrictions of the VAR(p).}
  \item{call}{The \code{call} to \command{VAR()}.}
}

\author{Bernhard Pfaff}

\references{
  Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
  University Press, Princeton.

  Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
    Analysis}, Springer, New York.
}

\seealso{
  \code{\link{summary}}, \code{\link{plot}},
  \code{\link{coef}}, \code{\link{residuals}},
  \code{\link{fitted}}, \code{\link{predict}}, \code{\link{irf}},
  \code{\link{fevd}}, \code{\link{Phi}}, \code{\link{Psi}},
  \code{\link{normality.test}}, \code{\link{arch.test}}, \code{\link{serial.test}},
  \code{\link{VARselect}}, \code{\link{logLik}} 
}

\examples{
data(Canada)
VAR(Canada, p = 2, type = "none")
VAR(Canada, p = 2, type = "const")
VAR(Canada, p = 2, type = "trend")
VAR(Canada, p = 2, type = "both")
}

\keyword{regression}
\concept{VAR}
\concept{Vector autoregressive model}
\concept{VAR}

