"plot.varstabil" <- 
function (x, plot.type = c("multiple", "single"), names = NULL, main = NULL, nc, mar = par("mar"), oma = par("oma"), ...) 
{
  op <- par(no.readonly = TRUE)
  plot.type <- match.arg(plot.type)
  ynames <- x$names
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
  
  ifelse(is.null(main), main <- paste(x$stability[[1]]$type, "of equation", names), main <- rep(main, nv)[1:nv])

  if (plot.type == "single") {
    par(mar = mar, oma = oma)
    if (nv > 1) par(ask = TRUE)
    for (i in 1:nv) {
      plot(x[[1]][[names[i]]], main = main[i], ...)
    }
  } else if(plot.type == "multiple") {
    if (missing(nc)) {
      nc <- ifelse(nv > 4, 2, 1)
    }
    nr <- ceiling(nv/nc)
    par(mfcol = c(nr, nc), mar = mar, oma = oma)
    for (i in 1:nv) {
      plot(x[[1]][[names[i]]], main = main[i], ...)
    }
  }
  on.exit(par(op))
}
