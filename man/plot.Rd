\name{plot}

\alias{plot.varcheck}

\alias{plot.varest}

\alias{plot.varfevd}

\alias{plot.varirf}

\alias{plot.varprd}

\alias{plot.varstabil}

\alias{plot.vec2var}

\encoding{latin1}

\title{Plot methods for objects in vars}

\description{
  Plot method for objects with class attribute \code{varest},
  \code{vec2var}, \code{varcheck}, \code{varfevd}, \code{varirf},
  \code{varprd}, \code{varstabil}. 
}

\usage{
\method{plot}{varcheck}(x, names = NULL, main.resid = NULL, main.hist =
NULL, main.acf = NULL, main.pacf = NULL, main.acf2 = NULL, main.pacf2 =
NULL, ylim.resid = NULL, ylim.hist = NULL, ylab.resid = NULL, xlab.resid
= NULL, xlab.acf = NULL, lty.resid = NULL, lwd.resid = NULL, col.resid =
NULL, col.edf = NULL, lag.acf = NULL, lag.pacf = NULL, lag.acf2 = NULL,
lag.pacf2 = NULL, mar = par("mar"), oma = par("oma"), ...)
\method{plot}{varest}(x, names = NULL, main.fit = NULL, main.acf = NULL,
main.pacf = NULL, ylim.fit = NULL, ylim.resid = NULL, lty.fit = NULL,
lty.resid = NULL, lwd.fit = NULL, lwd.resid = NULL, lag.acf = NULL,
lag.pacf = NULL, col.fit = NULL, col.resid = NULL, ylab.fit = NULL,
ylab.resid = NULL, ylab.acf = NULL, ylab.pacf = NULL, xlab.fit = NULL,
xlab.resid = NULL, nc, mar = par("mar"), oma = par("oma"), adj.mtext =
NA, padj.mtext = NA, col.mtext = NA, ...)
\method{plot}{vec2var}(x, ...)
\method{plot}{varfevd}(x, plot.type = c("multiple", "single"),
names = NULL, main = NULL, col = NULL, ylim = NULL, ylab = NULL,
xlab = NULL, legend = NULL, names.arg = NULL, nc,
mar = par("mar"), oma = par("oma"), addbars = 1, ...)
\method{plot}{varirf}(x, plot.type = c("multiple", "single"), names =
NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, col = NULL, ylim
= NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 4, 0, 4),
oma.multi = c(6, 4, 6, 4), adj.mtext = NA, padj.mtext = NA, col.mtext =
NA, ...)  
\method{plot}{varprd}(x, plot.type = c("multiple", "single"),
names = NULL, main = NULL, col = NULL, lty = NULL, lwd = NULL,
ylim = NULL, ylab = NULL, xlab = NULL, nc, mar = par("mar"),
oma = par("oma"), ...)
\method{plot}{varstabil}(x, plot.type = c("multiple", "single"), names =
NULL, main = NULL, nc, mar = par("mar"), oma = par("oma"), ...)
}

\arguments{
  \item{addbars}{Integer, number of empty bars in barplot to reserve
    space for legend. If set to zero, no legend will be returned.}
  \item{adj.mtext}{Adjustment for \code{mtext()}, only applicable if \code{plot.type = "multiple"}.}
  \item{col}{Character vector, colors to use in plot.}
  \item{col.edf}{Character, color of residuals' EDF.}
  \item{col.fit}{Character vector, colors for diagram of fit.}
  \item{col.mtext}{Character, color for \code{mtext()}, only applicable
  if \code{plot.type = "multiple"}.} 
  \item{col.resid}{Character vector, colors for residual plot.}
  \item{lag.acf}{Integer, lag.max for ACF of residuals.}
  \item{lag.acf2}{Integer, lag.max for ACF of squared residuals.}
  \item{lag.pacf}{Integer, lag.max for PACF of residuals.}
  \item{lag.pacf2}{Integer, lag.max for PACF of squared residuals.}
  \item{legend}{Character vector of names in legend.}
  \item{lty}{Integer/Character, the line types.}
  \item{lty.fit}{Vector, lty for diagram of fit.}
  \item{lty.resid}{Vector, lty for residual plot.}
  \item{lwd}{The width of the lines.}
  \item{lwd.fit}{Vector, lwd for diagram of fit.}
  \item{lwd.resid}{Vector, lwd for residual plot.}
  \item{main}{Character vector, the titles of the plot.}
  \item{main.acf}{Character vector, main for residuals' ACF.}
  \item{main.acf2}{Character vector, main for squared residuals' ACF.}
  \item{main.fit}{Character vector, main for diagram of fit.}
  \item{main.hist}{Character vector, main for histogram of residuals.}
  \item{main.pacf}{Character vector, main for residuals' PACF.}
  \item{main.pacf2}{Character vector, main for squared residuals' PACF.}
  \item{main.resid}{Character vector, main for residual plot.}
  \item{mar}{Setting of margins.}
  \item{mar.multi}{Setting of margins, if \code{plot.type = "multiple"}.}
  \item{names}{Character vector, the variables names to be plotted. If
    left \code{NULL}, all variables are plotted.} 
  \item{names.arg}{Character vector, names for x-axis of barplot.}
  \item{nc}{Integer, number of columns for multiple plot.}
  \item{oma}{Setting of outer margins.}
  \item{oma.multi}{Setting of margins, if \code{plot.type = "multiple"}.}
  \item{padj.mtext}{Adjustment for \code{mtext()}, only applicable if
  \code{plot.type = "multiple"}.} 
  \item{plot.type}{Character, if \code{multiple} all plots are drawn in
    a single device, otherwise the plots are shown consecutively.}
  \item{sub}{Character, sub title in plot.}
  \item{x}{An object of one of the above classes.}
  \item{xlab}{Character vector signifying the labels for the x-axis.}
  \item{xlab.acf}{Character, xlab for ACF and PACF of residuals and
    their squares in plot.varcheck.}
  \item{xlab.fit}{Character vector, xlab for diagram of fit.}
  \item{xlab.resid}{Character vector, xlab for residual plot.}  
  \item{ylab}{Character vector signifying the labels for the y-axis.}
  \item{ylab.acf}{Character, ylab for ACF.}
  \item{ylab.fit}{Character vector, ylab for diagram of fit.}
  \item{ylab.pacf}{Character, ylab for PACF}
  \item{ylab.resid}{Character vector, ylab for residual plot.}
  \item{ylim}{Vector, the limits of the y-axis.}
  \item{ylim.fit}{Vector, ylim for diagram of fit.}
  \item{ylim.hist}{Vector, ylim for histogram of residuals.}
  \item{ylim.resid}{Vector, ylim for residual plot.}
  \item{\dots}{Passed to internal plot function.}
}

\details{
  The plot-method for objects with class attribute \code{vec2var} is the
  same as for objects with class attribute \code{varest}. Hence, the
  same arguments can be utilised.
}

\author{Bernhard Pfaff}

\references{
  Hamilton, J. (1994), \emph{Time Series Analysis}, Princeton
  University Press, Princeton.

  Lütkepohl, H. (2006), \emph{New Introduction to Multiple Time Series
    Analysis}, Springer, New York.

  Zeileis, A., F. Leisch, K. Hornik and C. Kleiber (2002), strucchange:
   An R Package for Testing for Structural Change in Linear Regression
   Models, \emph{Journal of Statistical Software}, \bold{7(2)}: 1-38,
   \url{http://www.jstatsoft.org/v07/i02/}  
}

\seealso{
  \code{\link{VAR}}, \code{\link{vec2var}}, \code{\link{fevd}},
  \code{\link{irf}}, \code{\link{predict}}, \code{\link{fanchart}},
  \code{\link{stability}}, \code{\link{arch.test}}, \code{\link{normality.test}},
  \code{\link{serial.test}}  
}

\examples{
\dontrun{
data(Canada)
var.2c <- VAR(Canada, p = 2, type = "const")
plot(var.2c)
## Diagnostic Testing
## ARCH test
archtest <- arch.test(var.2c)
plot(archtest)
## Normality test
normalitytest <- normality.test(var.2c)
plot(normalitytest)
## serial correlation test
serialtest <- serial.test(var.2c)
plot(serialtest)
## FEVD
var.2c.fevd <- fevd(var.2c, n.ahead = 5)
plot(var.2c.fevd)
## IRF
var.2c.irf <- irf(var.2c, impulse = "e",
response = c("prod", "rw", "U"), boot = FALSE)
plot(var.2c.irf)
## Prediction
var.2c.prd <- predict(var.2c, n.ahead = 8, ci = 0.95)
plot(var.2c.prd)
## Stability
var.2c.stabil <- stability(var.2c, type = "Rec-CUSUM")
plot(var.2c.stabil)
}
}

\keyword{regression}
