\name{roots}

\alias{roots}

\encoding{latin1}

\title{Eigenvalues of the companion coefficient matrix of a VAR(p)-process}

\description{
  Returns a vector of the eigenvalues of the companion coefficient matrix.
}

\usage{
roots(x, modulus = TRUE)
}

\arguments{
  \item{x}{An object of class \sQuote{\code{varest}}, generated by
    \command{VAR()}.}
  \item{modulus}{Logical, set to \code{TRUE} for returning the modulus.}
}

\details{
  Any VAR(p)-process can be written in a first-order vector
  autoregressive form: the companion form. A VAR(p)-process is stable,
  if its reverse characteristic polynomial:

  \deqn{
    \det(I_K - A_1 z - \cdots - A_p z^p) \neq 0 \; \hbox{for} \; |z| \le 1
    \; ,
    }

  has no roots in or on the complex circle. This is equivalent to the
  condition that all eigenvalues of the companion matrix \eqn{A} have
  modulus less than 1. The function \code{roots()}, does compute the
  eigen values of the companion matrix \eqn{A} and returns by default
  their moduli.
}

\value{
  A vector object with the eigen values of the companion matrix, or
  their moduli (default).
}

\author{Bernhard Pfaff}

\references{
  Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
  University Press, Princeton.

  L�tkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
    Analysis}, Springer, New York.
}

\seealso{
  \code{\link{VAR}}
}

\examples{
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
roots(var.2c)
}

\keyword{regression}
