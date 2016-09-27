logLik.varest <- function(object, ...){
  obs <- object$obs
  df <- min(unlist(lapply(object$varresult, function(x) summary(x)$df[2])))
  K <- object$K
  resids <- resid(object)
  Sigma <- crossprod(resids) / obs
  r <- -( obs * K / 2 ) * log(2 * pi) - (obs / 2) * log(det(Sigma)) - (1 / 2) * sum(diag(resids %*% solve(Sigma) %*% t(resids)))
  class(r) <- "logLik"
  params <- sum(unlist(lapply(object$varresult, function(x) length(coef(x)))))
  attr(r, "df") <- params
  attr(r, "nobs") <- object$obs
  return(r)
}
