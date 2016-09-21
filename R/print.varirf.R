"print.varirf" <-
function(x, ...){
  cat("\nImpulse response coefficients\n")
  print(x[[1]], ...)
  if(x$boot){
    cat(paste("\nLower Band, CI=", 1 - x$ci, "\n"))
    print(x[[2]], ...)
    cat(paste("\nUpper Band, CI=", 1 - x$ci, "\n"))
    print(x[[3]], ...)
  }
}
