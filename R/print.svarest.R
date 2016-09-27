"print.svarest" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  text1 <- "SVAR Estimation Results:"
  cat(paste("\n", text1, "\n", sep = ""))
  row <- paste(rep("=", nchar(text1)), collapse = "")
  cat(row, "\n")
  cat("\n")
  if(identical(x$type, "Blanchard-Quah")){
    cat("\nEstimated contemporaneous impact matrix:\n")
    print(x$B, digits = digits, ...)
    cat("\nEstimated identified long run impact matrix:\n")
    print(x$LRIM, digits = digits, ...)
  } else if(identical(x$type, "A-model")){
    cat("\nEstimated A matrix:\n")
    print(x$A, digits = digits, ...)
  } else if(identical(x$type, "B-model")){
    cat("\nEstimated B matrix:\n")
    print(x$B, digits = digits, ...)
  } else if(identical(x$type, "AB-model")){
    cat("\nEstimated A matrix:\n")
    print(x$A, digits = digits, ...)
    cat("\nEstimated B matrix:\n")
    print(x$B, digits = digits, ...)
  }
  invisible(x)
}
