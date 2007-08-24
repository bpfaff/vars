"plot.varirf" <- 
function (x, plot.type = c("multiple", "single"), names = NULL, main = NULL, lty = NULL, lwd = NULL, col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar = par("mar"), oma = par("oma"), ...) 
{
  op <- par(no.readonly = TRUE)
  plot.type <- match.arg(plot.type)
  inames <- x$impulse
  rnames <- x$response
  if (is.null(names)) {
    names <- inames
  } else {
    names <- as.character(names)
    if (!(all(names %in% inames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      inames <- inames[1]
    } else {
      inames <- names
    }
  }
  
  nvi <- length(inames)
  nvr <- length(rnames)

  ifelse(is.null(lty), lty <- c(1, 1, 2, 2), lty <- rep(lty, 4)[1:4])
  ifelse(is.null(lwd), lwd <- c(1, 1, 1, 1), lwd <- rep(lwd, 4)[1:4])
  ifelse(is.null(col), col <- c("black", "gray", "red", "red"), col <- rep(col, 4)[1:4])
  ifelse(is.null(ylab), ylab <- "", ylab <- ylab)
  ifelse(is.null(xlab), xlab <- "", xlab <- xlab) 

  plotirf <- function(x, iname, rname, main, ylim, ...){
    if(is.null(main)){
      if ((x$model == "varest") || (x$model == "vec2var")) {
        if (x$ortho) {
          main <- paste("Orthogonal Impulse Response from", iname, "to", rname, sep = " ")
        } else {
          main <- paste("Impulse Response from", iname, "to", rname, sep = " ")
        }
      }
      else if (x$model == "svarest") {
        main <- paste("SVAR Impulse Response from", iname, "to", rname, sep = " ")
      } else if (x$model == "svecest") {
        main <- paste("SVECM Impulse Response from", iname, "to", rname, sep = " ")
      }
      if (x$cumulative)
        main <- paste(main, "(cumulative)", sep = " ")
    }
    if(is.null(ylim)){
      ymin <- min(c(x$irf[[iname]], x$Lower[[iname]], x$Upper[[iname]]))
      ymax <- max(c(x$irf[[iname]], x$Lower[[iname]], x$Upper[[iname]]))
      ylim <- c(ymin, ymax)
    }
    
    plot.ts(x$irf[[iname]][, rname], ylab = ylab, xlab = xlab, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], main = main, ...)
    abline(h = 0, col = col[2], lty = lty[2], lwd = lwd[2])
    if (x$boot) {
      lines(x$Lower[[iname]][, rname], col = col[3], lty = lty[3], lwd = lwd[3])
      lines(x$Upper[[iname]][, rname], col = col[4], lty = lty[4], lwd = lwd[4])
      mtext(paste((1 - x$ci) * 100, "% Bootstrap CI, ", x$runs, "runs"), side = 1, line = 2, outer = FALSE)
    }
  }
  for(i in 1:nvi){
    if(plot.type == "single"){
      if(nvr > 1) par(ask = TRUE)
      par(mar = mar, oma = oma)
    } else if(plot.type == "multiple"){
      if (missing(nc)) {
        nc <- ifelse(nvr > 4, 2, 1)
      }
      nr <- ceiling(nvr/nc)
      par(mfcol = c(nr, nc), mar = mar, oma = oma)
      if(nvi > 1) par(ask = TRUE)
    }
    for(j in 1:nvr){
      plotirf(x, iname = inames[i], rname = rnames[j], main = main, ylim = ylim, ...)
    }
  }
  on.exit(par(op))
}
