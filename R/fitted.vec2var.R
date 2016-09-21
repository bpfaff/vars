"fitted.vec2var" <-
function(object, ...){
  if (!(class(object) == "vec2var")) {
    stop("\nPlease, provide object of class 'vec2var' as 'object'.\n")
  }
  resids <- resid(object)
  fitted <- object$datamat[, colnames(object$y)] - resids
  colnames(fitted) <- paste("fit of", colnames(object$y))
  return(fitted)   
}
