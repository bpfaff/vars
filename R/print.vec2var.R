"print.vec2var" <-
function(x, ...){
  cat("\nCoefficient matrix of lagged endogenous variables:\n")
  for(i in 1:x$p){
    cat(paste("\nA", i, ":\n", sep = ""))
    print(x$A[[i]], ...)
    cat("\n")
  }
  cat("\nCoefficient matrix of deterministic regressor(s).\n")
  cat("\n")
  print(x$deterministic, ...)
  invisible(x)                                                                              
}

