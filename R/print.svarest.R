"print.svarest" <-
function(x, ...){
  title <- paste("# SVAR:", x$type, "#", sep=" ")
  row <- paste(rep("#", nchar(title)), collapse="")
  cat("\n")
  cat(row, "\n")
  cat(title, "\n")
  cat(row, "\n")
  cat("\n")
  if(identical(x$type, "Blanchard-Quah")){
    cat("\nEstimated contemporaneous impact matrix:\n")
    print(x$B)
    cat("\nEstimated identified long run impact matrix:\n")
    print(x$LRIM)
    invisible(x)
  } else {
    if(!is.null(x$LR)){
      cat("\nLR overidentification test:\n")
      print(x$LR)
    }
    cat("\nEstimated A matrix:\n")
    print(x$A)
    if(!is.null(x$Ase)){
      cat("\nEstimated standard errors for A matrix:\n")
      print(x$Ase)
    }
    cat("\nEstimated B matrix:\n")
    print(x$B)
    if(!is.null(x$Bse)){
      cat("\nEstimated standard errors for B matrix:\n")
      print(x$Bse)
    }
    invisible(x)
  }
}
