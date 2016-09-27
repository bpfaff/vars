"fitted.varest" <-
function(object, ...){
  return(sapply(object$varresult, fitted))
}
