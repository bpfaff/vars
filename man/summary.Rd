\name{summary}

\alias{summary}

\alias{summary.varest}

\alias{print.varsum}

\title{Summary method for objects of class varest}

\description{
  Returns the \sQuote{\code{summary.lm}} objects of the VAR equations.
}

\usage{
\method{summary}{varest}(object, ...)
}

\arguments{
  \item{object}{Object of class \sQuote{\code{varest}}.}
  \item{\dots}{dots currently not used.}
}

\value{
  Returns a list of \sQuote{\code{summary.lm}} objects.
}

\author{Bernhard Pfaff}

\seealso{
  \code{\link{VAR}}
}

\examples{
data(Canada)
var.2c <- VAR(Canada, p = 2 , type = "const")
summary(var.2c)
}

\keyword{regression}
