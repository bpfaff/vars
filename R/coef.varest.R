"coef.varest" <-
function(object, ...){
  return(lapply(lapply(object$varresult, summary), coef))
}
