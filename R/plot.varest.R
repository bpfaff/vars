"plot.varest" <- 
function (x, names = NULL, main.fit = NULL, main.resid = NULL, main.acf = NULL, main.pacf = NULL, ylim.fit = NULL, ylim.resid = NULL, lty.fit = NULL, lty.resid = NULL, lwd.fit = NULL, lwd.resid = NULL, lag.acf = NULL, lag.pacf = NULL, col.fit = NULL, col.resid = NULL, ylab.fit = NULL, ylab.resid = NULL, ylab.acf = NULL, ylab.pacf = NULL, xlab.fit = NULL, xlab.resid = NULL, nc, mar = par("mar"), oma = par("oma"), ...) 
{
  op <- par(no.readonly = TRUE)
  K <- x$K
  resids <- resid(x)
  fitted <- fitted(x)
  y <- x$datamat[, 1:K]
  ynames <- colnames(y)
  if (is.null(names)) {
    names <- ynames
  } else {
    names <- as.character(names)
    if (!(all(names %in% ynames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      names <- ynames[1]
    }
  }
  nv <- length(names)
  
  ifelse(is.null(main.fit), main.fit <- paste("Diagram of fit", names), main.fit <- rep(main.fit, nv)[1:nv])  
  ifelse(is.null(main.resid), main.resid <- rep("Residuals", nv), main.resid <- rep(main.resid, nv)[1:nv])  
  ifelse(is.null(main.acf), main.acf <- rep("ACF Residuals", nv), main.acf <- rep(main.acf, nv)[1:nv])  
  ifelse(is.null(main.pacf), main.pacf <- rep("PACF Residuals", nv), main.pacf <- rep(main.pacf, nv)[1:nv])
  ifelse(is.null(lty.fit), lty.fit <- c(1, 2), lty.fit <- rep(lty.fit, 2)[1:2])
  ifelse(is.null(lty.resid), lty.resid <- c(1, 1), lty.resid <- rep(lty.resid, 2)[1:2])
  ifelse(is.null(lwd.fit), lwd.fit <- c(1, 1), lwd.fit <- rep(lwd.fit, 2)[1:2])
  ifelse(is.null(lwd.resid), lwd.resid <- c(1, 1), lwd.resid <- rep(lwd.resid, 2)[1:2])
  ifelse(is.null(lag.acf), lag.acf <- 12, lag.acf <- lag.acf)
  ifelse(is.null(lag.pacf), lag.pacf <- 12, lag.pacf <- lag.pacf)
  ifelse(is.null(col.fit), col.fit <- c("black", "blue"), col.fit <- rep(col.fit, 2)[1:2])
  ifelse(is.null(col.resid), col.resid <- c("black", "red"), col.resid <- rep(col.resid, 2)[1:2])
  ifelse(is.null(ylab.fit), ylab.fit <- rep("", nv), ylab.fit <- rep(ylab.fit, nv)[1:nv])
  ifelse(is.null(ylab.resid), ylab.resid <- rep("", nv), ylab.resid <- rep(ylab.resid, nv)[1:nv])
  ifelse(is.null(ylab.acf), ylab.acf <- rep("", nv), ylab.acf <- rep(ylab.acf, nv)[1:nv])
  ifelse(is.null(ylab.pacf), ylab.pacf <- rep("", nv), ylab.pacf <- rep(ylab.pacf, nv)[1:nv])
  ifelse(is.null(xlab.fit), xlab.fit <- rep("", nv), xlab.fit <- rep(xlab.fit, nv)[1:nv])
  ifelse(is.null(xlab.resid), xlab.resid <- rep("", nv), xlab.resid <- rep(xlab.resid, nv)[1:nv])

  plotest <- function(y, fitted, resids, main.fit, main.resid, main.acf, main.pacf, ylab.fit, ylab.resid, ylab.acf, ylab.pacf, xlab.fit, xlab.resid, ...){
    ifelse(is.null(ylim.fit), ylim.fit <- c(min(c(y, fitted)), max(c(y, fitted))), ylim.fit <- ylim.fit)
    ifelse(is.null(ylim.resid), ylim.resid <- c(min(resids), max(resids)), ylim.resid <- ylim.resid)    
    layout(matrix(c(1, 1, 2, 2, 3, 4), nrow = 3, ncol = 2, byrow = TRUE))
    plot.ts(y, main = main.fit, ylim = ylim.fit, ylab = ylab.fit, xlab = xlab.fit, lty = lty.fit[1], lwd = lwd.fit[1], col = col.fit[1], ...)
    lines(fitted, col = col.fit[2], lty = lty.fit[2], lwd = lwd.fit[2])
    plot.ts(resids, main = main.resid, ylim = ylim.resid, ylab = ylab.resid, xlab = xlab.resid, lty = lty.resid[1], lwd = lwd.resid[1], col = col.resid[1], ...)
    abline(h = 0, col = col.resid[2], lty = lty.resid[2], lwd = lwd.resid[2])
    acf(resids, main = main.acf, ylab = ylab.acf, lag.max = lag.acf, ...)
    pacf(resids, main = main.pacf, ylab = ylab.pacf, lag.max = lag.pacf, ...)
  }
  par(mar = mar, oma = oma)
  if (nv > 1) par(ask = TRUE)
  for (i in 1:nv) {
    plotest(y = y[, i], fitted = fitted[, i], resids = resids[, i], main.fit = main.fit[i], main.resid = main.resid[i], main.acf = main.acf[i], main.pacf = main.pacf[i], ylab.fit = ylab.fit[i], ylab.resid = ylab.resid[i], ylab.acf = ylab.acf[i], ylab.pacf = ylab.pacf[i], xlab.fit = xlab.fit[i], xlab.resid = xlab.resid[i], ...)
  }
  on.exit(par(op))
}
