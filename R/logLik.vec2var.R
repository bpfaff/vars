logLik.vec2var <- function(object, ...){
    obs <- object$obs
    K <- object$K
    resids <- resid(object)
    Sigma <- crossprod(resids) / obs
    r <- -(obs * K/2) * log(2 * pi) - (obs/2) * log(det(Sigma)) - 
        (1/2) * sum(diag(resids %*% solve(Sigma) %*% t(resids)))
    class(r) <- "logLik"
    params <- length(unlist(object$A)) + length(object$deterministic)
    attr(r, "df") <- params
    attr(r, "nobs") <- object$obs
    return(r)
}
