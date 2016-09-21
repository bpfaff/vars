\name{VARselect}

\alias{VARselect}

\title{Information criteria and FPE for different VAR(p)}

\encoding{latin1}

\description{
  The function returns infomation criteria and final prediction error
  for sequential increasing the lag order up to a VAR(p)-proccess.
  which are based on the same sample size. 
}

\usage{
VARselect(y, lag.max = 10, type = c("const", "trend", "both", "none"),
season = NULL, exogen = NULL)
}

\arguments{
  \item{y}{Data item containing the endogenous variables}
  \item{lag.max}{Integer for the highest lag order (default is
    \code{lag.max = 10}).}
  \item{type}{Type of deterministic regressors to include.}
  \item{season}{Inlusion of centered seasonal dummy variables (integer
    value of frequency).}
  \item{exogen}{Inlusion of exogenous variables.}
  
}

\details{
  Estimates a VAR by OLS per equation. The model is of the following
  form:

  \deqn{
    \bold{y}_t = A_1 \bold{y}_{t-1} + \ldots + A_p \bold{y}_{t-p} + CD_t
    + \bold{u}_t
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
  Based on the same sample size the following information criteria and
  the final prediction error are computed:
  \deqn{
    AIC(n) = \ln \det(\tilde{\Sigma}_u(n)) + \frac{2}{T}n K^2 \quad, 
  }
  \deqn{
    HQ(n) = \ln \det(\tilde{\Sigma}_u(n)) + \frac{2 \ln(\ln(T))}{T}n K^2 \quad,
  }
  \deqn{
    SC(n) = \ln \det(\tilde{\Sigma}_u(n)) + \frac{\ln(T)}{T}n K^2 \quad,
  }

  \deqn{
    FPE(n) = \left ( \frac{T + n^*}{T - n^*} \right )^K
    \det(\tilde{\Sigma}_u(n)) \quad ,
  }
  with \eqn{\tilde{\Sigma}_u (n) = T^{-1} \sum_{t=1}^T \bold{\hat{u}}_t
    \bold{\hat{u}}_t'} and \eqn{n^*} is the total number of the
  parameters in each equation and \eqn{n} assigns the lag order.  
}

\value{
  A list with the following elements:\cr
  
  \item{selection}{Vector with the optimal lag number according to each
    criterium.}  
  \item{criteria}{A matrix containing the values of the criteria up to
    \code{lag.max}.} 
}

\author{Bernhard Pfaff}

\references{

  Akaike, H. (1969), Fitting autoregressive models for prediction,
  \emph{Annals of the Institute of Statistical Mathematics},
  \bold{21}: 243-247.

  Akaike, H. (1971), Autoregressive model fitting for control,
  \emph{Annals of the Institute of Statistical Mathematics},
  \bold{23}: 163-180.

  Akaike, H. (1973), Information theory and an extension of the maximum
  likelihood principle, in B. N. Petrov and F. Csáki (eds.), \emph{2nd
    International Symposium on Information Theory}, Académia Kiadó,
  Budapest, pp. 267-281.

  Akaike, H. (1974), A new look at the statistical model identification,
  \acronym{IEEE} \emph{Transactions on Automatic Control}, \bold{AC-19}:
  716-723.
  
  Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
  University Press, Princeton.

  Hannan, E. J. and B. G. Quinn (1979), The determination of the order
  of an autoregression, \emph{Journal of the Royal Statistical Society},
  \bold{B41}: 190-195.
  
  Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
    Analysis}, Springer, New York.

  Quinn, B. (1980), Order determination for a multivariate
  autoregression, \emph{Journal of the Royal Statistical Society},
  \bold{B42}: 182-185.

  Schwarz, G. (1978), Estimating the dimension of a model, \emph{Annals
    of Statistics}, \bold{6}: 461-464.
}

\seealso{
  \code{\link{VAR}}
}

\examples{
data(Canada)
VARselect(Canada, lag.max = 5, type="const")
}

\keyword{regression}
\concept{VAR}
\concept{Vector autoregressive model}
\concept{Information criteria}
\concept{Final Prediction Error}
\concept{Akaike}
\concept{Hannan-Quinn}
\concept{Schwarz}
