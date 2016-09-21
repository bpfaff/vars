"residuals.varest" <-
function(object, ...){
  return(sapply(object$varresult, residuals))
}
