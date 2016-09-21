"residuals.vec2var" <-
function(object, ...){
  if (!(class(object) == "vec2var")) {
    stop("\nPlease, provide object of class 'vec2var' as 'object'.\n")
  }
  resids <- object$datamat[, colnames(object$y)] - object$datamat[, colnames(object$deterministic)] %*% t(object$deterministic)
  for(i in 1:object$p){
    resids <- resids - object$datamat[, colnames(object$A[[i]])] %*% t(object$A[[i]])
  }
  colnames(resids) <- paste("resids of", colnames(object$y))
  return(resids)   
}
