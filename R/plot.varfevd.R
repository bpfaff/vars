"plot.varfevd" <-
function (x, plot.type = c("multiple", "single"), names = NULL, main = NULL, col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, legend = NULL, names.arg = NULL, nc, mar = par("mar"), oma = par("oma"), addbars = 1, ...) 
{
  K <- length(x)
  ynames <- names(x)
  plot.type <- match.arg(plot.type)
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
  op <- par(no.readonly = TRUE)
  ifelse(is.null(main), main <- paste("FEVD for", names), main <- rep(main, nv)[1:nv])
  ifelse(is.null(col), col <- gray.colors(K), col <- rep(col, K)[1:K])
  ifelse(is.null(ylab), ylab <- rep("Percentage", nv), ylab <- rep(ylab, nv)[1:nv])
  ifelse(is.null(xlab), xlab <- rep("Horizon", nv), xlab <- rep(xlab, nv)[1:nv])
  ifelse(is.null(ylim), ylim <- c(0, 1), ylim <- ylim)
  ifelse(is.null(legend), legend <- ynames, legend <- legend)
  if(is.null(names.arg)) names.arg <- c(paste(1:nrow(x[[1]])), rep(NA, addbars))
  plotfevd <- function(x, main, col, ylab, xlab, names.arg, ylim, ...){
    addbars <- as.integer(addbars)
    if(addbars > 0){
      hmat <- matrix(0, nrow = K, ncol = addbars)
      xvalue <- cbind(t(x), hmat)
      barplot(xvalue, main = main, col = col, ylab = ylab, xlab = xlab, names.arg = names.arg, ylim = ylim, legend.text = legend, ...)
      abline(h = 0)
    } else {
      xvalue <- t(x)
      barplot(xvalue, main = main, col = col, ylab = ylab, xlab = xlab, names.arg = names.arg, ylim = ylim, ...)
      abline(h = 0)
    }
  }
  if (plot.type == "single") {
    par(mar = mar, oma = oma)
    if (nv > 1) par(ask = TRUE)
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col, ylab = ylab[i], xlab = xlab[i], names.arg = names.arg, ylim = ylim, ...)
    }
  } else if (plot.type == "multiple") {
    if (missing(nc)) {
      nc <- ifelse(nv > 4, 2, 1)
    }
    nr <- ceiling(nv / nc)
    par(mfcol = c(nr, nc), mar = mar, oma = oma)    
    for (i in 1:nv) {
      plotfevd(x = x[[names[i]]], main = main[i], col = col, ylab = ylab[i], xlab = xlab[i], names.arg = names.arg, ylim = ylim, ...)
    }
  }
  on.exit(par(op))
}
