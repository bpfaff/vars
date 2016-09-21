"logLik.svecest" <- 
function (object, ...) 
{
    K <- object$var@P
    A <- diag(K)
    B <- object$SR
    obs <- nrow(object$var@Z0)
    Sigma <- object$Sigma.U / 100
    r <- -(K * obs/2) * log(2 * pi) + obs/2 * log(det(A)^2) - 
        obs/2 * log(det(B)^2) - obs/2 * sum(diag(t(A) %*% solve(t(B)) %*% 
        solve(B) %*% A %*% Sigma))
    class(r) <- "logLik"
    return(r)
}
