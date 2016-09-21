\name{Canada}

\docType{data}

\alias{Canada}

\title{Canada: Macroeconomic time series}

\description{
  The original time series are published by the \acronym{OECD}. The
  sample range is from the 1stQ 1980 until 4thQ 2000. The following
  series have been utilised in the construction of the series provided
  in \code{Canada}:\cr
  \cr
  \emph{Main Economic Indicators:}\cr
  \tabular{ll}{
    Canadian unemployment rate in \% \tab 444113DSA \cr
    Canadian manufacturing real wage \tab 444321KSA \cr
    Canadian consumer price index \tab 445241K \cr
  }
  \emph{Quarterly National Accounts:}\cr
  \tabular{ll}{
    Canadian nominal GDP \tab CAN1008S1\cr
  }
  \emph{Labour Force Statistics:}\cr
  \tabular{ll}{
    Canadian civil employment in 1000 persons \tab 445005DSA\cr
  }
  The series in \code{Canada} are constructed as:\cr
  \tabular{rl}{
  \code{prod} := \tab 100*(ln(CAN1008S1/445241K)-ln(445005DSA))\cr
  \code{e} := \tab 100*ln(445005DSA)\cr
  \code{U} := \tab 444113DSA\cr
  \code{rw} := \tab 100*ln(100*444321KSA)\cr
  }
  Hence, \code{prod} is used as a measure of labour productivity;
  \code{e} is used for employment; \code{U} is the unemployment rate and
  \code{rw} assigns the real wage.
}

\usage{Canada}

\format{An object with class attributes \code{mts} and \code{ts} containing four variables with 84 observations.}

\source{\acronym{OECD}: \url{http://www.oecd.org}; data set is available
  for download at \url{http://www.jmulti.org/data_atse.html}, the
  official homepage of \acronym{JMULTI} is \url{www.jmulti.com}.}

\keyword{datasets}
