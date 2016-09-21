"print.varest" <-
function(x, digits = max(3, getOption("digits") - 3), ...){
  dim <- length(x$varresult)
  names <- colnames(x$y)
  text1 <- "VAR Estimation Results:"
  cat(paste("\n", text1, "\n", sep = ""))
  row <- paste(rep("=", nchar(text1)), collapse = "")
  cat(row, "\n")
  cat("\n")
  for(i in 1:dim){
    result <- coef(x$varresult[[i]])
    text1 <- paste("Estimated coefficients for equation ", names[i], ":", sep = "")
    cat(text1, "\n")  
    row <- paste(rep("=", nchar(text1)), collapse="")
    cat(row, "\n")
    text2 <- paste("Call:\n", names[i], " = ", paste(names(result), collapse = " + "), sep = "")
    cat(text2, "\n\n")
    print(result, ...)
    cat("\n\n")
  }
  invisible(x)
}


