"print.svecest" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  text1 <- "SVEC Estimation Results:"
  cat(paste("\n", text1, "\n", sep = ""))
  row <- paste(rep("=", nchar(text1)), collapse = "")
  cat(row, "\n")
  cat("\n")
  cat("\nEstimated contemporaneous impact matrix:\n")
  print(x$SR, digits = digits, ...)
  cat("\nEstimated long run impact matrix:\n")
  print(x$LR, digits = digits, ...)
  invisible(x)
}
