"summary.svarest" <-
function(object, ...){
  type <- object$type
  obs <- nrow(object$var$datamat)
  A <- object$A
  B <- object$B
  Ase <- object$Ase
  Bse <- object$Bse
  LRIM <- object$LRIM
  logLik <- as.numeric(logLik(object))
  Sigma.U <- object$Sigma.U
  LR <- object$LR
  iter <- object$iter
  call <- object$call
  opt <- object$opt
  result <- list(type = type, A = A, B = B, Ase = Ase, Bse = Bse, LRIM = LRIM, Sigma.U = Sigma.U, logLik = logLik, LR = LR, obs = obs, opt = opt, iter = iter, call = call) 
  class(result) <- "svarsum"
  return(result)
}
