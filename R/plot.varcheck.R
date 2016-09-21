"plot.varcheck" <- 
function (x, names = NULL, main.resid = NULL, main.hist = NULL, main.acf = NULL, main.pacf = NULL, main.acf2 = NULL, main.pacf2 = NULL, ylim.resid = NULL, ylim.hist = NULL, ylab.resid = NULL, xlab.resid = NULL, xlab.acf = NULL, lty.resid = NULL, lwd.resid = NULL, col.resid = NULL, col.edf = NULL, lag.acf = NULL, lag.pacf = NULL, lag.acf2 = NULL, lag.pacf2 = NULL, mar = par("mar"), oma = par("oma"), ...) 
{
  op <- par(no.readonly = TRUE)
  rnames <- colnames(x$resid)
  if (is.null(names)) {
    names <- rnames
  } else {
    names <- as.character(names)
    if (!(all(names %in% rnames))) {
      warning("\nInvalid residual name(s) supplied, using residuals of first variable.\n")
      names <- rnames[1]
    }
  }
  nv <- length(names)
  resids <- matrix(x$resid[, names], ncol = nv)

  ifelse(is.null(main.resid), main.resid <- paste("Residuals of", names), main.resid <- rep(main.resid, nv)[1:nv])
  ifelse(is.null(main.hist), main.hist <- rep("Histogram and EDF", nv), main.hist <- rep(main.hist, nv)[1:nv])
  ifelse(is.null(main.acf), main.acf <- rep("ACF of Residuals", nv), main.acf <- rep(main.acf, nv)[1:nv])
  ifelse(is.null(main.pacf), main.pacf <- rep("PACF of Residuals", nv), main.pacf <- rep(main.pacf, nv)[1:nv])
  ifelse(is.null(main.acf2), main.acf2 <- rep("ACF of squared Residuals", nv), main.acf2 <- rep(main.acf2, nv)[1:nv])
  ifelse(is.null(main.pacf2), main.pacf2 <- rep("PACF of squared Residuals", nv), main.pacf2 <- rep(main.pacf2, nv)[1:nv])  
  ifelse(is.null(ylab.resid), ylab.resid <- rep("", nv), ylab.resid <- rep(ylab.resid, nv)[1:nv])
  ifelse(is.null(xlab.resid), xlab.resid <- rep("", nv), xlab.resid <- rep(xlab.resid, nv)[1:nv])
  ifelse(is.null(xlab.acf), xlab.acf <- rep("", nv), xlab.acf <- rep(xlab.acf, nv)[1:nv])
  ifelse(is.null(lty.resid), lty.resid <- c(1, 1), lty.resid <- rep(lty.resid, 2)[1:2])
  ifelse(is.null(lwd.resid), lwd.resid <- c(1, 1), lwd.resid <- rep(lwd.resid, 2)[1:2])
  ifelse(is.null(col.resid), col.resid <- c("black", "red"), col.resid <- rep(col.resid, 2)[1:2])  
  ifelse(is.null(col.edf), col.edf <- "blue", col.edf <- col.edf[1])
  ifelse(is.null(lag.acf), lag.acf <- 12, lag.acf <- lag.acf)
  ifelse(is.null(lag.pacf), lag.pacf <- 12, lag.pacf <- lag.pacf)
  ifelse(is.null(lag.acf2), lag.acf2 <- 12, lag.acf2 <- lag.acf2)
  ifelse(is.null(lag.pacf2), lag.pacf2 <- 12, lag.pacf2 <- lag.pacf2)
 
  plotcheck <- function(resid, main.resid, ylab.resid, xlab.resid, main.hist, main.acf, main.pacf, main.acf2, main.pacf2){
    ifelse(is.null(ylim.resid), ylim.resid <- c(min(resid), max(resid)), ylim.resid <- ylim.resid)
    ifelse(is.null(ylim.hist), ylim.hist <- c(0, 1), ylim.hist <- ylim.hist)    
    plot.ts(resid, main = main.resid, ylim = ylim.resid, ylab = ylab.resid, xlab = xlab.resid, lty = lty.resid[1], lwd = lwd.resid[2], col = col.resid[1], ...)
    abline(h = 0, col = col.resid[2], lty = lty.resid[2], lwd = lwd.resid[2])
    acf(resid, main = main.acf, ylab = "", xlab = xlab.acf, lag.max = lag.acf)
    acf(resid^2, main = main.acf2, ylab = "", xlab = xlab.acf, lag.max = lag.acf2)
    hist(resid, main = main.hist, freq = FALSE, xlab = "", ylim = ylim.hist)
    lines(density(resid), col = col.edf)
    pacf(resid, main = main.pacf, ylab = "", xlab = xlab.acf, lag.max = lag.pacf)
    pacf(resid^2, main = main.pacf2, ylab = "", xlab = xlab.acf, lag.max = lag.pacf2)
  }
    par(mfcol = c(3, 2), mar = mar, oma = oma)
    if (nv > 1) par(ask = TRUE)
    for (i in 1:nv) {
      plotcheck(resid = resids[, i], main.resid = main.resid[i], ylab.resid = ylab.resid[i], xlab.resid = xlab.resid[i], main.hist = main.hist[i], main.acf = main.acf[i], main.pacf = main.pacf[i], main.acf2 = main.acf2[i], main.pacf2 = main.pacf2[i])

    }
  on.exit(par(op))
}
