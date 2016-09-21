"print.svecsum" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  text1 <- "SVEC Estimation Results:"
  cat(paste("\n", text1, "\n", sep = ""))
  row <- paste(rep("=", nchar(text1)), collapse = "")
  cat(row, "\n")
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  cat(paste("Type:", x$type, "\n"))
  cat(paste("Sample size:", x$obs, "\n"))
  cat(paste("Log Likelihood:", round(x$logLik, 3), "\n"))
  cat(paste("Number of iterations:", x$iter, "\n"))   
  if(!is.null(x$LRover)){
    cat("\nLR overidentification test:\n")
    print(x$LRover, digits = digits, ...)
  }
  cat("\nEstimated contemporaneous impact matrix:\n")
  print(x$SR, digits = digits, ...)
  if(!is.null(x$SRse)){
    cat("\nEstimated standard errors for impact matrix:\n")
    print(x$SRse, digits = digits, ...)
  }
  cat("\nEstimated long run impact matrix:\n")
  print(x$LR, digits = digits, ...)
  if(!is.null(x$LRse)){
    cat("\nEstimated standard errors for long-run matrix:\n")
    print(x$LRse, digits = digits, ...)
  }
  cat("\nCovariance matrix of reduced form residuals (*100):\n")
  print(x$Sigma.U, digits = digits, ...)
  invisible(x)
}
