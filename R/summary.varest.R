"summary.varest" <-
function(object, equations = NULL, ...){
  ynames <- colnames(object$y)
  obs <- nrow(object$datamat)
  if (is.null(equations)) {
    names <- ynames
  }
  else {
    names <- as.character(equations)
    if (!(all(names %in% ynames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      names <- ynames[1]
    }
  }
  eqest <- lapply(object$varresult[names], summary)
  resids <- resid(object)
  covres <- cov(resids) * (obs - 1) / (obs - (ncol(object$datamat) - object$K))
  corres <- cor(resids)
  logLik <- as.numeric(logLik(object))
  roots <- roots(object)
  result <- list(names = names, varresult = eqest, covres = covres, corres = corres, logLik = logLik, obs = obs, roots = roots, type = object$type, call = object$call)
  class(result) <- "varsum"
  return(result)
}
  
