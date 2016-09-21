"summary.svecest" <-
function(object, ...){
  type <- object$type
  K <- object$var@P
  obs <- nrow(object$var@Z0)  
  SR <- object$SR
  LR <- object$LR
  SRse <- object$SRse
  LRse <- object$LRse
  logLik <- as.numeric(logLik(object))
  Sigma.U <- object$Sigma.U
  LRover <- object$LRover
  r <- object$r
  iter <- object$iter  
  call <- object$call
  result <- list(type = type, SR = SR, LR = LR, SRse = SRse, LRse = LRse, Sigma.U = Sigma.U, logLik = logLik, LRover = LRover, obs = obs, r = r, iter = iter, call = call)
  class(result) <- "svecsum"
  return(result)
}
