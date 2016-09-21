"plot.varprd" <-
function(x, plot.type = c("multiple", "single"), names = NULL, main = NULL, col = NULL, lty = NULL, lwd = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar = par("mar"), oma = par("oma"), ...){
  op <- par(no.readonly = TRUE)
  ynames <- colnames(x$endog)
  smpl <- nrow(x$endog)
  K <- ncol(x$endog)
  plot.type <- match.arg(plot.type)
  if(is.null(names)){
    names <- ynames
  } else {
    names <- as.character(names)
    if(!(all(names %in% ynames))){
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      names <- ynames[1]
    }
  }
  nv <- length(names)
  ifelse(is.null(main), main <- paste("Forecast of series", names), main <- rep(main, nv)[1:nv])
  ifelse(is.null(col), col <- c("blue", "black", "red", "red", "grey"), col <- rep(col, 5)[1:5])
  ifelse(is.null(lty), lty <- c(2, 1, 3, 3, 4), lty <- rep(lty, 5)[1:5])
  ifelse(is.null(lwd), lwd <- rep(1, 5), lwd <- rep(lwd, 5)[1:5])
  ifelse(is.null(ylab), ylab <- rep("", nv), ylab <- rep(ylab, nv)[1:nv])
  ifelse(is.null(xlab), xlab <- rep("", nv), xlab <- rep(xlab, nv)[1:nv])    
  plotprd <- function(x, name, main, col, lty, lwd, ylab, xlab, ...){
    fcsty <- c(rep(NA, smpl - 1), x$endog[smpl, name], x$fcst[[name]][, 1])
    fcstl <- c(rep(NA, smpl - 1), x$endog[smpl, name], x$fcst[[name]][, 2])
    fcstu <- c(rep(NA, smpl - 1), x$endog[smpl, name], x$fcst[[name]][, 3])
    smply <- c(x$endog[, name], rep(NA, length(x$fcst[[name]][, 1])))
    if(is.null(ylim)){
      min.y <- min(na.omit(c(fcsty, fcstl, fcstu, smply)))
      max.y <- max(na.omit(c(fcsty, fcstl, fcstu, smply)))
      ylim <- c(min.y, max.y)
    }
    plot.ts(fcsty, main = main, ylab = ylab, xlab = xlab, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], ...) 
    lines(smply, col = col[2], lty = lty[2], lwd = lwd[2])
    lines(fcstl, col = col[3], lty = lty[3], lwd = lwd[3])
    lines(fcstu, col = col[4], lty = lty[4], lwd = lwd[4])
    abline(v = smpl, col = col[5], lty = lty[5], lwd = lwd[5])
  }
  if(plot.type == "single"){
    par(mar = mar, oma = oma)
    if(nv > 1) par(ask = TRUE)
    for(i in 1:nv){
      plotprd(x = x, name = names[i], main = main[i], col = col, lty = lty, lwd = lwd, ylab = ylab[i], xlab = xlab[i], ...)
    }
  } else if(plot.type == "multiple"){
    if(missing(nc)){
      nc <- ifelse(nv > 4, 2, 1)
    }
    nr <- ceiling(nv / nc)
    par(mfcol = c(nr, nc), mar = mar, oma = oma)          
    for(i in 1:nv){
      plotprd(x = x, name = names[i], main = main[i], col = col, lty = lty, lwd = lwd, ylab = ylab[i], xlab = xlab[i], ...)
    }    
  }  
  on.exit(par(op))
}
