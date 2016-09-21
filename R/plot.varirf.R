"plot.varirf" <- 
function (x, plot.type = c("multiple", "single"), names = NULL, 
    main = NULL, sub = NULL, lty = NULL, lwd = NULL, col = NULL, ylim = NULL, 
    ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 4, 0, 4),
    oma.multi = c(6, 4, 6, 4), adj.mtext = NA, padj.mtext = NA, col.mtext = NA, ...)  
{
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    ##
    ## Checking of arguments
    ##
    plot.type <- match.arg(plot.type)
    inames <- x$impulse
    rnames <- x$response
    if (is.null(names)) {
        names <- inames
    }
    else {
        names <- as.character(names)
        if (!(all(names %in% inames))) {
            warning("\nInvalid variable name(s) supplied, using first variable.\n")
            inames <- inames[1]
        }
        else {
            inames <- names
        }
    }
    nvi <- length(inames)
    nvr <- length(rnames)
    ##
    ## Presetting certain plot-argument
    ##   
    ifelse(is.null(lty), lty <- c(1, 1, 2, 2), lty <- rep(lty, 4)[1:4])
    ifelse(is.null(lwd), lwd <- c(1, 1, 1, 1), lwd <- rep(lwd, 4)[1:4])
    ifelse(is.null(col), col <- c("black", "gray", "red", "red"), col <- rep(col, 4)[1:4])
    ##
    ## Extract data from object for plotting per iname
    ##
    dataplot <- function(x, iname){
      impulses <- x$irf[[iname]]
      range <- range(impulses)
      upper <- NULL
      lower <- NULL
      if(x$boot){
        upper <- x$Upper[[iname]]
        lower <- x$Lower[[iname]]
        range <- range(cbind(impulses, upper, lower))
      }
      if ((x$model == "varest") || (x$model == "vec2var")) {
        if (x$ortho) {
          text1 <- paste("Orthogonal Impulse Response from", iname, sep = " ")
        } else {
         text1 <- paste("Impulse Response from", iname, sep = " ")
        }
      } else if (x$model == "svarest") {
        text1 <- paste("SVAR Impulse Response from", iname, sep = " ")
      } else if (x$model == "svecest") {
        text1 <- paste("SVECM Impulse Response from", iname, sep = " ")
      }
      if (x$cumulative)  text1 <- paste(text1, "(cumulative)", sep = " ")
      text2 <- ""
      if (x$boot) text2 <- paste((1 - x$ci) * 100, "% Bootstrap CI, ", x$runs, "runs")
        
      result <- list(impulses = impulses, upper = upper, lower = lower, range = range, text1 = text1, text2 = text2)
      return(result)
    }
    ##
    ## Plot function for irf per impulse and response
    ##
    plot.single <- function(x, iname, rname, ...) {
      ifelse(is.null(main), main <- x$text1, main <- main)
      ifelse(is.null(sub), sub <- x$text2, sub <- sub)
      xy <- xy.coords(x$impulse[, rname])
      ifelse(is.null(ylab), ylabel <- rname, ylabel <- ylab)
      ifelse(is.null(xlab), xlabel <- "", xlabel <- xlab)
      ifelse(is.null(ylim), ylim <- x$range, ylim <- ylim)
      plot(xy, type = "l", ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], axes = FALSE, ylab = paste(ylabel), xlab = paste(xlab), ...)
      title(main = main, sub = sub, ...)
      axis(1, at = xy$x, labels = c(0:(length(xy$x) - 1)))
      axis(2, ...)
      box()    
      if (!is.null(x$upper)) lines(x$upper[, rname], col = col[3], lty = lty[3], lwd = lwd[3])
      if (!is.null(x$lower)) lines(x$lower[, rname], col = col[3], lty = lty[3], lwd = lwd[3])
      abline(h = 0, col = col[2], lty = lty[2], lwd = lwd[2])
    }
    ##
    ## Plot function per impulse
    ##
    plot.multiple <- function(dp, nc = nc, ...){
      x <- dp$impulses
      y <- dp$upper
      z <- dp$lower
      ifelse(is.null(main), main <- dp$text1, main <- main)
      ifelse(is.null(sub), sub <- dp$text2, sub <- sub)
      ifelse(is.null(ylim), ylim <- dp$range, ylim <- ylim)
      range <- range(c(x, y, z))
      nvr <- ncol(x)
      if (missing(nc)) {
        nc <- ifelse(nvr > 4, 2, 1)
      }
      nr <- ceiling(nvr/nc)
      par(mfrow = c(nr, nc), mar = mar.multi, oma = oma.multi)
      if(nr > 1){
        for(i in 1:(nvr - nc)){
          ifelse(is.null(ylab), ylabel <- colnames(x)[i], ylabel <- ylab)
          xy <- xy.coords(x[, i])
          plot(xy, axes = FALSE, type = "l", ylab = ylabel, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], ...)
          axis(2, at = pretty(range)[-1])
          abline(h = 0, col = "red")
          if(!is.null(y)) lines(y[, i], col = col[3], lty = lty[3], lwd = lwd[3])
          if(!is.null(z)) lines(z[, i], col = col[3], lty = lty[3], lwd = lwd[3])
          box()
        }       
        for(j in (nvr - nc + 1):nvr){
          ifelse(is.null(ylab), ylabel <- colnames(x)[j], ylabel <- ylab)
          xy <- xy.coords(x[, j])
          plot(xy, axes = FALSE, type = "l", ylab = ylabel, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], ...)
          axis(2, at = pretty(range)[-1])
          axis(1, at = 1:(nrow(x)), labels = c(0:(nrow(x) - 1)))
          box()
          abline(h = 0, col = "red")
          if(!is.null(y)) lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
          if(!is.null(z)) lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        }
        mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)
        mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)        
      } else {
        for(j in 1:nvr){
          ifelse(is.null(ylab), ylabel <- colnames(x)[j], ylabel <- ylab)
          xy <- xy.coords(x[, j])
          plot(xy, type = "l", ylab = ylabel, ylim = ylim, col = col[1], lty = lty[1], lwd = lwd[1], ...)
          if(!is.null(y)) lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
          if(!is.null(z)) lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
          abline(h = 0, col = "red")
        }
        mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)
        mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, padj = padj.mtext, col = col.mtext, ...)
      }
    }
    ##
    ## Plot for type = single
    ##
    if (plot.type == "single") {
      for(i in 1:nvi){
        dp <- dataplot(x, iname = inames[i]) 
        for(j in 1:nvr){
          plot.single(dp, iname = inames[i], rname = rnames[j], ...)
          if (nvr > 1) par(ask = TRUE)
        }
      }
    }
    ##
    ## Plot for type = multiple
    ##
    if (plot.type == "multiple") {
      for (i in 1:nvi) {
        dp <- dataplot(x, iname = inames[i])
        plot.multiple(dp, nc = nc, ...)
        if (nvi > 1) par(ask = TRUE)
      }
    }   
}

       
