"print.svarsum" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  text1 <- "SVAR Estimation Results:"
  cat(paste("\n", text1, "\n", sep = ""))
  row <- paste(rep("=", nchar(text1)), collapse = "")
  cat(row, "\n")
  cat("\nCall:\n")
  print(x$call)
  cat("\n")
  cat(paste("Type:", x$type, "\n"))
  cat(paste("Sample size:", x$obs, "\n"))
  cat(paste("Log Likelihood:", round(x$logLik, 3), "\n"))
  if(!(x$type == "Blanchard-Quah")){
    cat(paste("Method:", x$call$estmethod, "\n"))
    cat(paste("Number of iterations:", x$iter, "\n"))   
    if(x$call$estmethod == "direct"){
      cat(paste("Convergence code:", x$opt$convergence, "\n"))
      if(!is.null(x$opt$message)){
        print(x$opt$message)
      }
    }
  }
  if(!is.null(x$LR)){
    cat("\nLR overidentification test:\n")
    print(x$LR, digits = digits, ...)
  }
  if(identical(x$type, "Blanchard-Quah")){
    cat("\nEstimated contemporaneous impact matrix:\n")
    print(x$B, digits = digits, ...)
    cat("\nEstimated identified long run impact matrix:\n")
    print(x$LRIM, digits = digits, ...)
  } else {
    cat("\nEstimated A matrix:\n")
    print(x$A, digits = digits, ...)
    if(any(c(x$Ase) != 0)){
      cat("\nEstimated standard errors for A matrix:\n")
      print(x$Ase, digits = digits, ...)
    }
    cat("\nEstimated B matrix:\n")
    print(x$B, digits = digits, ...)
    if(any(c(x$Bse) != 0)){
      cat("\nEstimated standard errors for B matrix:\n")
      print(x$Bse, digits = digits, ...)
    }
  }
  cat("\nCovariance matrix of reduced form residuals (*100):\n")
  print(x$Sigma.U, digits = digits, ...)
  invisible(x)
}
