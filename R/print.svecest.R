"print.svecest" <-
function(x, ...){
  title <- paste("#", x$type, "#", sep=" ")
  row <- paste(rep("#", nchar(title)), collapse="")
  cat("\n")
  cat(row, "\n")
  cat(title, "\n")
  cat(row, "\n")
  cat("\n")
  if(!is.null(x$LRover)){
    cat("\nLR overidentification test:\n")
    print(x$LRover)
  }
  cat("\nEstimated contemporaneous impact matrix:\n")
  print(x$SR)
  if(!is.null(x$SRse)){
    cat("\nBootstrap standard errors for contemp. impact matrix:\n")
    print(x$SRse)
  }  
  cat("\nEstimated long run impact matrix:\n")
  print(x$LR)
  if(!is.null(x$LRse)){
    cat("\nBootstrap standard errors for long run impact matrix:\n")
    print(x$LRse)
  }
  invisible(x)
}
