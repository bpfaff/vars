"print.varsum" <-
function(x, ...){
  print(lapply(x, summary.lm))
  invisible(x)
}

