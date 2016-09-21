logLik.svarest <- function(object, ...){
  obs <- object$var$obs
  K <- object$var$K
  A <- object$A
  B <- object$B
  Sigma <- object$Sigma.U / 100
  r <- -(K * obs/2) * log(2 * pi) + obs/2 * log(det(A)^2) -  obs/2 * log(det(B)^2) - obs/2 * sum(diag(t(A) %*%  solve(t(B)) %*% solve(B) %*% A %*% Sigma))   
  class(r) <- "logLik"
  return(r)
}
