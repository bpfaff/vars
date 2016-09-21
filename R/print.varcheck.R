"print.varcheck" <-
function(x, ...){
  lapply(x[-1], print, ...)
}
