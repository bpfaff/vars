require(MASS)
require(strucchange)
##
## Forecast variance-covariance matrix (VAR)
##
".fecov" <-
function(x, n.ahead) {
  n.par<-sapply(x$varresult, function(x) summary(x)$df[2])
  sigma.u <- crossprod(resid(x))/n.par
  Sigma.yh <- array(NA, dim = c(x$K, x$K, n.ahead))
  Sigma.yh[, , 1] <- sigma.u
  Phi <- Phi(x, nstep = n.ahead)
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j] %*% sigma.u %*% t(Phi[, , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}
##
## Forecast variance-covariance matrix (SVAR)
##
".fecovsvar" <-
function(x, n.ahead) {
  Sigma.yh <- array(NA, dim = c(x$var$K, x$var$K, n.ahead))
  Phi <- Phi(x, nstep = n.ahead)
  Sigma.yh[, , 1] <- Phi[, , 1]%*%t(Phi[, , 1])
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = x$var$K, ncol = x$var$K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j]%*%t(Phi[, , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}
##
## Forecast variance-covariance matrix (vec2var)
##
".fecovvec2var" <-
function(x, n.ahead) {
  sigma.u <- crossprod(resid(x))/x$obs 
  Sigma.yh <- array(NA, dim = c(x$K, x$K, n.ahead))
  Sigma.yh[, , 1] <- sigma.u
  Phi <- Phi(x, nstep = n.ahead)
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = x$K, ncol = x$K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j] %*% sigma.u %*% t(Phi[, , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}
##
## Forecast variance-covariance matrix (SVEC)
##
".fecovsvec" <-
function(x, n.ahead, K) {
  Sigma.yh <- array(NA, dim = c(K, K, n.ahead))
  Phi <- Phi(x, nstep = n.ahead)
  Sigma.yh[, , 1] <- Phi[, , 1]%*%t(Phi[, , 1])
  if (n.ahead > 1) {
    for (i in 2:n.ahead) {
      temp <- matrix(0, nrow = K, ncol = K)
      for (j in 2:i) {
        temp <- temp + Phi[, , j]%*%t(Phi[, , j])
      }
      Sigma.yh[, , i] <- temp + Sigma.yh[, , 1]
    }
  }
  return(Sigma.yh)
}
##
## irf (internal)
##
".irf" <-
function(x, impulse, response, y.names, n.ahead, ortho, cumulative){
  if((class(x) == "varest") || (class(x) == "vec2var")){
    if(ortho){
      irf <- Psi(x, nstep = n.ahead)
    } else {
      irf <- Phi(x, nstep = n.ahead)
    }
  } else if((class(x) == "svarest") || (class(x) == "svecest")){
    irf <- Phi(x, nstep = n.ahead)
  }
  dimnames(irf) <- list(y.names, y.names, NULL)
  idx <- length(impulse)
  irs <- list()
  for(i in 1 : idx){
    irs[[i]] <- matrix(t(irf[response , impulse[i], 1 : (n.ahead + 1)]), nrow = n.ahead+1)
    colnames(irs[[i]]) <- response
    if(cumulative){
      if(length(response) > 1) irs[[i]] <- apply(irs[[i]], 2, cumsum)
      if(length(response) == 1){
        tmp <- matrix(cumsum(irs[[1]]))
        colnames(tmp) <- response
        irs[[1]] <- tmp
      }
    }
  }
  names(irs) <- impulse
  result <- irs
  return(result)    
}
##
## Bootstrapping IRF for VAR and SVAR
##
".boot" <-
function(x, n.ahead, runs, ortho, cumulative, impulse, response, ci, seed, y.names){
  if(!(is.null(seed))) set.seed(abs(as.integer(seed)))
  if(class(x) == "varest"){
    VAR <- eval.parent(x)
  }else if(class(x) == "svarest"){
    VAR <- eval.parent(x$var)
  } else {
    stop("Bootstrap not implemented for this class.\n")
  }
  p <- VAR$p
  K <- VAR$K
  obs <- VAR$obs
  total <- VAR$totobs
  type <- VAR$type
  B <- Bcoef(VAR)
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(VAR$y)
  Zdet <- NULL
  if(ncol(VAR$datamat) > (K * (p+1))){
    Zdet <- as.matrix(VAR$datamat[, (K * (p + 1) + 1):ncol(VAR$datamat)])
  }
  resorig <- scale(resid(VAR), scale = FALSE)
  B <- Bcoef(VAR)
  for(i in 1:runs){
    booted <- sample(c(1 : obs), replace=TRUE)
    resid <- resorig[booted, ]
    lasty <- c(t(VAR$y[p : 1, ]))
    ysampled[c(1 : p), ] <- VAR$y[c(1 : p), ]
    for(j in 1 : obs){
      lasty <- lasty[1 : (K * p)]
      Z <- c(lasty, Zdet[j, ])
      ysampled[j + p, ] <- B %*% Z + resid[j, ]
      lasty <- c(ysampled[j + p, ], lasty) 
    }
    varboot <- update(VAR, y = ysampled)
    if(class(x) == "svarest"){
      varboot <- update(x, x = varboot)
    }
    BOOT[[i]] <- .irf(x = varboot, n.ahead = n.ahead, ortho = ortho, cumulative = cumulative, impulse = impulse, response = response, y.names=y.names)
  }
  lower <- ci / 2
  upper <- 1 - ci / 2
  mat.l <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  mat.u <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  Lower <- list()
  Upper <- list()
  idx1 <- length(impulse)
  idx2 <- length(response)
  idx3 <- n.ahead + 1
  temp <- rep(NA, runs)
  for(j in 1 : idx1){
    for(m in 1 : idx2){
      for(l in 1 : idx3){
        for(i in 1 : runs){
          if(idx2 > 1){
            temp[i] <- BOOT[[i]][[j]][l, m]
          } else {
            temp[i] <- matrix(BOOT[[i]][[j]])[l, m]
          }
        }
        mat.l[l, m] <- quantile(temp, lower, na.rm = TRUE)
        mat.u[l, m] <- quantile(temp, upper, na.rm = TRUE)
      }
    }
    colnames(mat.l) <- response
    colnames(mat.u) <- response
    Lower[[j]] <- mat.l
    Upper[[j]] <- mat.u
  }
  names(Lower) <- impulse
  names(Upper) <- impulse
  result <- list(Lower = Lower, Upper = Upper)
  return(result)
}
##
## Bootstrapping coefficients SVEC
##
.bootsvec <- function(x, LRorig, SRorig, r, runs, K, conv.crit, maxls, max.iter){
  ##
  ## Obtaining level-VAR
  ##
  varlevel <- vec2var(x, r = r)
  Resids <- scale(varlevel$resid, scale = FALSE)
  obs <- varlevel$obs
  totobs <- varlevel$totobs 
  P <- totobs - obs
  ##
  ## Fixing beta
  ##
  betafix <- matrix(x@V[, 1:r], ncol = r)
  ##
  ## Computing the coefficient matrix
  ##
  coeffmat <- cbind(varlevel$deterministic, matrix(unlist(varlevel$A), nrow = K))
  ##
  ## Initialising the BOOT matrix, the sampled y 
  ## and the deterministic regressors
  ##
  BOOT <- matrix(0, nrow = 2*K^2, ncol = runs)
  ysampled <- matrix(0, nrow = totobs, ncol = K)
  Zdet <- varlevel$datamat[, -c(1:K)]
  nrhs <- ncol(Zdet)
  ndet <- nrhs - K*P
  Zdet <- matrix(Zdet[, 1:ndet], nrow = obs, ncol = ndet)
  ##
  ## Conducting the Bootstrap
  ##
  for(i in 1:runs){
    ##
    ## Sampling of the residuals
    ##
    booted <- sample(c(1 : obs), replace=TRUE)
    resid <- Resids[booted, ]
    ##
    ## Setting the starting values for y
    ##
    lasty <- c(t(varlevel$y[P : 1, ]))
    ysampled[c(1 : P), ] <- varlevel$y[c(1 : P), ]
    for(j in 1 : obs){
      lasty <- lasty[1 : (K * P)]
      Z <- c(Zdet[j, ], lasty)
      ysampled[j + P, ] <- coeffmat %*% Z + resid[j, ]
      lasty <- c(ysampled[j + P, ], lasty) 
    }
    colnames(ysampled) <- colnames(x@x)
    ##
    ## Re-estimating the VECM
    ##
    ifelse(is.null(x@call$K), Korig <- 2, Korig <- x@call$K)
    ifelse(is.null(x@call$spec), specorig <- "longrun", specorig <- x@call$spec)
    if(is.null(x@call$season)){
      seasonorig <- NULL
    }else {
      seasonorig <- x@call$season
    }
    if(is.null(x@call$dumvar)){
      dumvarorig <- NULL
    }else {
      dumvarorig <- x@call$dumvar
    }
    ifelse(is.null(x@call$ecdet), ecdetorig <- "none", ecdetorig <- x@call$ecdet)    
    vecm <- ca.jo(x = ysampled, K = Korig, spec = specorig, season = seasonorig, dumvar = dumvarorig, ecdet = ecdetorig)
    vecm@V <- betafix
    ##
    ## Re-estimating the SVEC
    ##
    svec <- SVEC(x = vecm, LR = LRorig, SR = SRorig, r = r, max.iter = max.iter, conv.crit = conv.crit, maxls = maxls, boot = FALSE, lrtest = FALSE)
    SRboot <- c(svec$SR)
    LRboot <- c(svec$LR)
    bootvals <- c(SRboot, LRboot)
    ##
    ## Storing the parameters in BOOT
    ##
    BOOT[, i] <- bootvals
  }
  return(BOOT)
}
##
## Bootstrapping IRF for Level-VECM
##
.bootirfvec2var <- function(x = x, n.ahead = n.ahead, runs = runs, ortho = ortho, cumulative = cumulative, impulse = impulse, response = response, ci = ci, seed = seed, y.names = y.names){
  ##
  ## Obtaining VECM arguments
  ##
  vecm <- x$vecm
  vecm.ecdet <- vecm@ecdet
  vecm.season <- vecm@season
  vecm.dumvar <- vecm@dumvar
  vecm.K <- vecm@lag
  vecm.spec <- vecm@spec   
  ##
  ## Getting VAR-level coefficients
  ##
  Zdet <- matrix(x$datamat[ , colnames(x$deterministic)], ncol = ncol(x$deterministic))
  B <- x$deterministic
  for(i in 1:x$p){
    B <- cbind(B, x$A[[i]])
  }
  p <- x$p
  K <- x$K
  obs <- x$obs
  total <- x$totobs
  ##
  ## Initialising Bootstrap
  ##
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(x$y)
  resorig <- scale(resid(x), scale = FALSE)
  ##
  ## Conducting the bootstrapping
  ##
  for(i in 1:runs){
    booted <- sample(c(1 : obs), replace=TRUE)
    resid <- resorig[booted, ]
    lasty <- c(t(x$y[p : 1, ]))
    ysampled[c(1 : p), ] <- x$y[c(1 : p), ]
    ##
    ## Obtaining the new y
    ##
    for(j in 1 : obs){
      lasty <- lasty[1 : (K * p)]
      Z <- c(Zdet[j, ], lasty)
      ysampled[j + p, ] <- B %*% Z + resid[j, ]
      lasty <- c(ysampled[j + p, ], lasty)
    }
    vec <- ca.jo(ysampled, ecdet = vecm.ecdet, season = vecm.season, dumvar = vecm.dumvar, K = vecm.K, spec = vecm.spec)
    varboot <- vec2var(vec, r = x$r)
    BOOT[[i]] <- .irf(x = varboot, n.ahead = n.ahead, ortho = ortho, cumulative = cumulative, impulse = impulse, response = response, y.names = y.names)
  }
  ##
  ## Obtaining the lower and upper bounds
  ##
  lower <- ci / 2
  upper <- 1 - ci / 2
  mat.l <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  mat.u <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  Lower <- list()
  Upper <- list()
  idx1 <- length(impulse)
  idx2 <- length(response)
  idx3 <- n.ahead + 1
  temp <- rep(NA, runs)
  for(j in 1 : idx1){
    for(m in 1 : idx2){
      for(l in 1 : idx3){
        for(i in 1 : runs){
          if(idx2 > 1){
            temp[i] <- BOOT[[i]][[j]][l, m]
          } else {
            temp[i] <- matrix(BOOT[[i]][[j]])[l, m]
          }
        }
        mat.l[l, m] <- quantile(temp, lower, na.rm = TRUE)
        mat.u[l, m] <- quantile(temp, upper, na.rm = TRUE)
      }
    }
    colnames(mat.l) <- response
    colnames(mat.u) <- response
    Lower[[j]] <- mat.l
    Upper[[j]] <- mat.u    
  }
  result <- list(Lower = Lower, Upper = Upper)
  return(result)
}
##
## Bootstrapping IRF for SVEC
##
.bootirfsvec <- function(x = x, n.ahead = n.ahead, runs = runs, ortho = ortho, cumulative = cumulative, impulse = impulse, response = response, ci = ci, seed = seed, y.names = y.names){
  ##
  ## Obtaining VECM arguments
  ##
  vecm <- x$var
  vecm.ecdet <- vecm@ecdet
  vecm.season <- vecm@season
  vecm.dumvar <- vecm@dumvar
  vecm.K <- vecm@lag
  vecm.spec <- vecm@spec
  vecm.beta <- vecm@V
  ##
  ## Obtaining arguments for SVEC
  ##
  svec.r <- x$r
  if(is.null(x$call$start)){
    svec.start <- NULL
  } else {
    svec.start <- x$call$start
  }
  ifelse(is.null(x$call$max.iter), svec.maxiter <- 100, svec.maxiter <- x$call$max.iter)
  ifelse(is.null(x$call$conv.crit), svec.convcrit <- 1e-07, svec.convcrit <- x$call$conv.crit)
  ifelse(is.null(x$call$maxls), svec.maxls <- 1.0, svec.maxls <- x$call$maxls)
  ##
  ## Getting VAR-level coefficients
  ##
  varlevel <- vec2var(vecm, r = svec.r)
  Zdet <- matrix(varlevel$datamat[ , colnames(varlevel$deterministic)], ncol = ncol(varlevel$deterministic))
  B <- varlevel$deterministic
  for(i in 1:varlevel$p){
    B <- cbind(B, varlevel$A[[i]])
  }
  p <- varlevel$p
  K <- varlevel$K
  obs <- varlevel$obs
  total <- varlevel$totobs
  ##
  ## Initialising Bootstrap
  ##
  BOOT <- vector("list", runs)
  ysampled <- matrix(0, nrow = total, ncol = K)
  colnames(ysampled) <- colnames(varlevel$y)
  resorig <- scale(varlevel$resid, scale = FALSE)
  ##
  ## Conducting the bootstrapping
  ##
  for(i in 1:runs){
    booted <- sample(c(1 : obs), replace=TRUE)
    resid <- resorig[booted, ]
    lasty <- c(t(varlevel$y[p : 1, ]))
    ysampled[c(1 : p), ] <- varlevel$y[c(1 : p), ]
    ##
    ## Obtaining the new y
    ##
    for(j in 1 : obs){
      lasty <- lasty[1 : (K * p)]
      Z <- c(Zdet[j, ], lasty)
      ysampled[j + p, ] <- B %*% Z + resid[j, ]
      lasty <- c(ysampled[j + p, ], lasty)
    }
    vec <- ca.jo(ysampled, ecdet = vecm.ecdet, season = vecm.season, dumvar = vecm.dumvar, K = vecm.K, spec = vecm.spec)
    ##vec@V <- vecm.beta
    svec <- SVEC(x = vec, LR = x$LRorig, SR = x$SRorig, r = svec.r, max.iter = svec.maxiter, maxls = svec.maxls, lrtest = FALSE, boot = FALSE)
    BOOT[[i]] <- .irf(x = svec, n.ahead = n.ahead, cumulative = cumulative, ortho = ortho, impulse = impulse, response = response, y.names = y.names)    
  }
  ##
  ## Obtaining the lower and upper bounds
  ##
  lower <- ci / 2
  upper <- 1 - ci / 2
  mat.l <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  mat.u <- matrix(NA, nrow = n.ahead + 1, ncol = length(response))
  Lower <- list()
  Upper <- list()
  idx1 <- length(impulse)
  idx2 <- length(response)
  idx3 <- n.ahead + 1
  temp <- rep(NA, runs)
  for(j in 1 : idx1){
    for(m in 1 : idx2){
      for(l in 1 : idx3){
        for(i in 1 : runs){
          if(idx2 > 1){
            temp[i] <- BOOT[[i]][[j]][l, m]
          } else {
            temp[i] <- matrix(BOOT[[i]][[j]])[l, m]
          }
        }
        mat.l[l, m] <- quantile(temp, lower, na.rm = TRUE)
        mat.u[l, m] <- quantile(temp, upper, na.rm = TRUE)
      }
    }
    colnames(mat.l) <- response
    colnames(mat.u) <- response
    Lower[[j]] <- mat.l
    Upper[[j]] <- mat.u
  }  
  result <- list(Lower = Lower, Upper = Upper)
  return(result)
}
##
## univariate ARCH test
##
".arch.uni" <-
function(x, lags.single){
  lags.single <- lags.single + 1
  mat <- embed(scale(x)^2, lags.single)
  arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
  STATISTIC <- arch.lm$r.squared*length(resid(arch.lm))
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- lags.single - 1
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "ARCH test (univariate)"
  result <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = deparse(substitute(x)))
  class(result) <- "htest"
  return(result)
}
##
## multivariate ARCH test
##
".arch.multi" <-
function(x, lags.multi, obj.name, K, obs){
  col.arch.df <- 0.5 * K * (K + 1)
  arch.df <- matrix(NA, nrow = obs, ncol = col.arch.df)
  for( i in 1 : obs){
    temp <- outer(x[i,], x[i,])
    arch.df[i,] <- temp[lower.tri(temp, diag=TRUE)]
  }
  lags.multi <- lags.multi + 1
  arch.df <- embed(arch.df, lags.multi)
  archm.lm0 <- lm(arch.df[ , 1:col.arch.df] ~ 1)
  archm.lm0.resids <- resid(archm.lm0)
  omega0 <- cov(archm.lm0.resids)
  archm.lm1 <- lm(arch.df[ , 1 : col.arch.df] ~ arch.df[ , -(1 : col.arch.df)])
  archm.lm1.resids <- resid(archm.lm1)
  omega1 <- cov(archm.lm1.resids)
  R2m <- 1 - (2 / (K * (K + 1))) * sum(diag(omega1 %*% solve(omega0)))
  n <- nrow(archm.lm1.resids)
  STATISTIC <- 0.5 * n * K * (K+1) * R2m
  names(STATISTIC) <- "Chi-squared"
  lags.multi <- lags.multi - 1
  PARAMETER <- lags.multi * K^2 * (K + 1)^2 / 4
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "ARCH (multivariate)"
  result <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(result) <- "htest"
  return(result)
}
##
## univariate normality test
##
".jb.uni" <-
function(x, obs){
  x <- as.vector(x)
  m1 <- sum(x) / obs
  m2 <- sum((x - m1)^2) / obs
  m3 <- sum((x - m1)^3) / obs
  m4 <- sum((x - m1)^4) / obs
  b1 <- (m3 / m2^(3 / 2))^2
  b2 <- (m4/m2^2)
  STATISTIC <- obs * b1 / 6 + obs * (b2 - 3)^2 / 24
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- 2
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = 2)
  METHOD <- "JB-Test (univariate)"
  result <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = deparse(substitute(x)))
  class(result) <- "htest"
  return(result)
}
##
## multivariate normality test
##
".jb.multi" <-
function(x, obs, K, obj.name){
  P <- chol(crossprod(x) / obs)
  resids.std <- x %*% solve(P)
  b1 <- apply(resids.std, 2, function(x) sum(x^3) / obs)
  b2 <- apply(resids.std, 2, function(x) sum(x^4) / obs)
  s3 <- obs * t(b1) %*% b1 / 6
  s4 <- obs * t(b2 - rep(3, K)) %*% (b2 - rep(3, K)) / 24
  STATISTIC <- s3 + s4
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- 2 * K
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "JB-Test (multivariate)"
  result1 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(result1) <- "htest"
  STATISTIC <- s3
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- K
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "Skewness only (multivariate)"
  result2 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(result2) <- "htest"
  STATISTIC <- s4
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- K
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "Kurtosis only (multivariate)"
  result3 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(result3) <- "htest"
  result <- list(JB = result1, Skewness = result2, Kurtosis = result3)
  return(result)
}
##
## Convenience function for computing lagged x
##
".matlag1" <-
function(x, lag = 1){
  totcols <- ncol(x)
  nas <- matrix(NA, nrow = lag, ncol = totcols)
  x <- rbind(nas, x)
  totrows <- nrow(x)
  x <- x[-c((totrows - lag + 1) : totrows), ]
  return(x)
}
##
## Multivariate Portmanteau Statistic
##
".pt.multi" <-
function(x, K, obs, lags.pt, obj.name, resids){
  C0 <- crossprod(resids) / obs
  C0inv <- solve(C0)
  tracesum <- rep(NA, lags.pt)
  for(i in 1 : lags.pt){
    Ut.minus.i <- .matlag1(resids, lag = i)[-c(1 : i), ]
    Ut <- resids[-c(1 : i), ]
    Ci <- crossprod(Ut, Ut.minus.i) / obs
    tracesum[i] <- sum(diag(t(Ci) %*% C0inv %*% Ci %*% C0inv))
  }
  vec.adj <- obs - (1 : lags.pt)
  Qh <- obs * sum(tracesum)
  Qh.star <- obs^2 * sum(tracesum / vec.adj)
  nstar <- K^2 * x$p
  ## htest objects for Qh and Qh.star
  STATISTIC <- Qh
  names(STATISTIC) <- "Chi-squared"
  if(identical(class(x), "varest")){
    PARAMETER <- (K^2 * lags.pt - nstar)
  } else {
    PARAMETER <- (K^2 * lags.pt - nstar + x$K)
  }    
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "Portmanteau Test (asymptotic)"
  PT1 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(PT1) <- "htest"
  STATISTIC <- Qh.star
  names(STATISTIC) <- "Chi-squared"
  if(identical(class(x), "varest")){
    PARAMETER <- (K^2 * lags.pt - nstar)
  } else {
    PARAMETER <- (K^2 * lags.pt - nstar + x$K)
  }        
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "Portmanteau Test (adjusted)"
  PT2 <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(PT2) <- "htest"
  result <- list(PT1 = PT1, PT2 = PT2)
  return(result)
}
##
## Breusch-Godfrey and Edgerton-Shukur Test
##
".bgserial" <-
function(x, K, obs, lags.bg, obj.name, resids){
  ylagged <- as.matrix(x$datamat[, -c(1 : K)])
  resids.l <- .matlag2(resids, lag = lags.bg)
  if(is.null(x$restrictions)){
    regressors <- as.matrix(cbind(ylagged, resids.l))
    lm0 <- lm(resids ~ -1 + regressors)
    lm1 <- lm(resids ~ -1 + ylagged)
    sigma.1 <- crossprod(resid(lm1)) / obs
    sigma.0 <- crossprod(resid(lm0)) / obs
  } else {
    resid0 <- matrix(NA, ncol = K, nrow = obs)
    resid1 <- matrix(NA, ncol = K, nrow = obs)
    for(i in 1 : K){
      datares <- data.frame(ylagged[, which(x$restrictions[i, ] == 1)])
      regressors <- data.frame(cbind(datares, resids.l))
      lm0 <- lm(resids[, i] ~ -1 + ., data=regressors)
      lm1 <- lm(resids[, i] ~ -1 + ., data=datares)
      resid0[, i] <- resid(lm0)
      resid1[, i] <- resid(lm1)
      sigma.0 <- crossprod(resid0) / obs
      sigma.1 <- crossprod(resid1) / obs
    }
  }
  LMh.stat <- obs * (K - sum(diag(crossprod(solve(sigma.1), sigma.0))))
  STATISTIC <- LMh.stat
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- lags.bg * K^2
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "Breusch-Godfrey LM test"
  LMh <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(LMh) <- "htest"
  ## small sample correction of Edgerton Shukur
  R2r <- 1 - det(sigma.0) / det(sigma.1)
  m <- K * lags.bg
  q <- 0.5 * K * m - 1
  n <- ncol(x$datamat) - K
  N <- obs - n - m - 0.5 * (K - m + 1)
  r <- sqrt((K^2 * m^2 - 4)/(K^2 + m^2 - 5))
  LMFh.stat <- (1 - (1 - R2r)^(1 / r))/(1 - R2r)^(1 / r) * (N * r - q) / (K * m)
  STATISTIC <- LMFh.stat
  names(STATISTIC) <- "F statistic"
  PARAMETER1 <- lags.bg * K^2
  names(PARAMETER1) <- "df1"
  PARAMETER2 <- floor(N * r - q)
  names(PARAMETER2) <- "df2"
  PVAL <-   1 - pf(LMFh.stat, PARAMETER1, PARAMETER2)
  METHOD <- "Edgerton-Shukur F test"
  LMFh <- list(statistic = STATISTIC, parameter = c(PARAMETER1, PARAMETER2), p.value = PVAL, method = METHOD, data.name = paste("Residuals of VAR object", obj.name))
  class(LMFh) <- "htest"
  return(list(LMh = LMh, LMFh = LMFh))
}
##
## Duplication matrix
##
".duplicate" <-
function(n){
  D <- matrix(0, nrow = n^2, ncol = n * (n + 1) / 2)
  count <- 0
  for(j in 1 : n){
    D[(j - 1) * n + j, count + j] <- 1
    if((j + 1) <= n){
      for(i in (j + 1):n){
        D[(j - 1) * n + i, count + i] <- 1
        D[(i - 1) * n + j, count + i] <- 1
      }
    }
    count <- count + n - j
  }
  return(D)
}
##
## Convenience function for computing lagged residuals
##
".matlag2" <-
function(x, lag = 1){
  K <- ncol(x)
  obs <- nrow(x)
  zeromat <- matrix(0, nrow = obs, ncol = K * lag)
  idx1 <- seq(1, K * lag, K)
  idx2 <- seq(K, K * lag, K)
  for(i in 1:lag){
    lag <- i + 1
    res.tmp <- embed(x, lag)[, -c(1 : (K * i))]
    zeromat[-c(1 : i), idx1[i] : idx2[i]] <- res.tmp
  }
  resids.l <- zeromat
  return(resids.l)
}
