"SVEC" <-
function(x, LR = NULL, SR = NULL, r = 1, start = NULL, max.iter = 100, conv.crit = 1e-07, maxls = 1, lrtest = TRUE, boot = FALSE, runs = 100){
  if (!(class(x) == "ca.jo")) {
    stop("\nPlease, provide object of class 'ca.jo' as 'z'.\n")
  }
  if((is.null(LR)) || (is.null(SR))){
    stop("Please, provide matrix objects for 'LR' and 'SR'.\n")
  }
  r <- as.integer(r)
  if(!({1 <= r} && {r < ncol(x@x)})){
    stop(paste("\nThe cointegration rank 'r' must be in the interval [1:", ncol(z@x) - 1, "].\n", sep = ""))
  }
  ##
  ## Setting parameters and identity matrices
  ##
  K <- x@P
  K2 <- K^2
  P <- x@lag - 1
  IK <- diag(K)
  IK2 <- diag(K2)
  Kkk <- diag(K2)[, c(sapply(1:K, function(i) seq(i, K2, K)))]
  svartype <- "B-model"
  ##
  ## Checking for correct dimensions of LR and SR
  ##
  if(!identical(dim(LR), as.integer(c(K, K)))){
    stop(paste("Dimension of 'LR' must be (", K, "x", K, ")."))
  }
  if(!identical(dim(SR), as.integer(c(K, K)))){
    stop(paste("Dimension of 'SR' must be (", K, "x", K, ")."))
  }
  SRorig <- SR
  LRorig <- LR
  ##
  ## Obtaining restricted VECM with normalised beta
  ##
  vecr <- cajorls(z = x, r = r)
  ##
  ## Obtaining Sigma of vecr
  ##
  Resids <- residuals(vecr$rlm)
  obs <- nrow(Resids)
  Sigma <- crossprod(Resids) / obs
  ##
  ## Obtaining Xi matrix
  ##
  Coef.vecr <- coef(vecr$rlm)
  alpha <- t(Coef.vecr[1:r, ])
  ifelse(r == 1, alpha.orth <- Null(t(alpha)), alpha.orth <- Null(alpha))
  beta <- vecr$beta[1:K, ]
  beta.orth <- Null(beta)
  Gamma.array <- array(c(t(tail(coef(vecr$rlm), K*P))), c(K, K, P))
  Gamma <- apply(Gamma.array, c(1, 2), sum)
  Xi <- beta.orth %*% solve(t(alpha.orth) %*% (IK - Gamma) %*% beta.orth) %*% t(alpha.orth)
  ##
  ## S-Matrix for explicit form
  ##
  Lrres <- sum(is.na(LR))
  SRres <- sum(is.na(SR))
  R0 <- diag(K^2)
  select <- c(apply(SR, c(1, 2), function(x) ifelse(identical(x, 0.0), TRUE, FALSE)))
  R.B <- R0[select, ]  
  select <- c(apply(LR, c(1, 2), function(x) ifelse(identical(x, 0.0), TRUE, FALSE)))  
  R.C1 <- R0[select, ]
  if(identical(nrow(R.C1), as.integer(0))){
    R.C1 <- matrix(0, nrow = K2, ncol = K2)
    nResC1 <- 0
  }  
  R.C1 <- R.C1 %*% kronecker(IK, Xi)
  nResC1 <- qr(R.C1)$rank
  ##
  ## Setting up the R matrix (implicit form)
  ##
  if(identical(nrow(R.B), as.integer(0))){
    R <- R.C1
    nResB <- 0
  } else {
    R <- rbind(R.C1, R.B)
    nResB <- qr(R.B)$rank
  }
  ##
  ## Obtaining the S matrix and s vector (explicit form)
  ##
  Sb <- Null(t(R))
  S <- rbind(matrix(0, nrow = K^2, ncol = ncol(Sb)), Sb)
  l<- ncol(S)
  s <- c(c(diag(K)), rep(0, K^2))
  ##
  ## Test of unidentification
  ##
  if((nResB + nResC1) < (K * (K - 1) / 2)){
    stop("The model is not identified. Use less free parameters.")
  }
  ##
  ## Test identification numerically
  ##
  ifelse(is.null(start), gamma <- start <- rnorm(l), gamma <- start)
  vecab <- S %*% gamma + s
  A <- matrix(vecab[1:K2], nrow = K, ncol = K)
  B <- matrix(vecab[(K2 + 1):(2 * K2)], nrow = K, ncol = K)
  v1 <- (IK2 + Kkk) %*% kronecker(t(solve(A) %*% B), solve(B))
  v2 <- -1 * (IK2 + Kkk) %*% kronecker(IK, solve(B))
  v <- cbind(v1, v2)
  idmat <- v %*% S
  ms <- t(v) %*% v
  auto <- eigen(ms)$values
  rni <- 0
  for (i in 1:l) {
    if (auto[i] < 1e-11)
      rni <- rni + 1
  }
  if (identical(rni, 0)) {
    if (identical(l, as.integer(K * (K + 1)/2))) {
      ident <- paste("The", svartype, "is just identified.")
    } else {
      ident <- paste("The", svartype, "is over identified.")
    }
  } else {
    ident <- paste("The", svartype, "is unidentified. The non-identification rank is", rni, ".")
    stop(ident)
  }      
  ##
  ## Scoring Algorithm
  ##
  iters <- 0
  cvcrit <- conv.crit + 1
  while (cvcrit > conv.crit) {
    z <- gamma
    vecab <- S %*% gamma + s
    A <- matrix(vecab[1:K2], nrow = K, ncol = K)
    B <- matrix(vecab[(K2 + 1):(2 * K2)], nrow = K, ncol = K)
    Binv <- solve(B)
    Btinv <- solve(t(B))
    BinvA <- Binv %*% A
    infvecab.mat1 <- rbind(kronecker(solve(BinvA), Btinv), -1 * kronecker(IK, Btinv))
    infvecab.mat2 <- IK2 + Kkk
    infvecab.mat3 <- cbind(kronecker(t(solve(BinvA)), Binv), -1 * kronecker(IK, Binv))
    infvecab <- obs * (infvecab.mat1 %*% infvecab.mat2 %*% infvecab.mat3)
    infgamma <- t(S) %*% infvecab %*% S
    infgammainv <- solve(infgamma)
    scorevecBinvA <- obs * c(solve(t(BinvA))) - obs * (kronecker(Sigma, IK) %*% c(BinvA))
    scorevecAB.mat <- rbind(kronecker(IK, Btinv), -1 * kronecker(BinvA, Btinv))
    scorevecAB <- scorevecAB.mat %*% scorevecBinvA
    scoregamma <- t(S) %*% scorevecAB
    direction <- infgammainv %*% scoregamma
    length <- max(abs(direction))
    ifelse(length > maxls, lambda <- maxls/length, lambda <- 1)
    gamma <- gamma + lambda * direction
    iters <- iters + 1
    z <- z - gamma
    cvcrit <- max(abs(z))
    if (iters >= max.iter) {
      warning(paste("Convergence not achieved after", iters, "iterations. Convergence value:", cvcrit, "."))
      break
    }
  }
  iter <- iters - 1
  vecab <- S %*% gamma + s
  SR <- B
  ##
  ## Normalising the sign of SR
  ##
  select <- which(diag(solve(A) %*% B) < 0)
  SR[, select] <- -1 * SR[, select]
  ##
  ## Computing LR and Sigma.U
  ##
  LR <- Xi %*% SR
  Sigma.U <- solve(A) %*% B %*% t(B) %*% t(solve(A))
  colnames(SR) <- colnames(x@x)
  rownames(SR) <- colnames(SR)
  colnames(LR) <- colnames(SR)
  rownames(LR) <- colnames(SR)
  colnames(Sigma.U) <- colnames(SR)
  rownames(Sigma.U) <- colnames(SR)
  ##
  ## LR overidentification test
  ##
  LRover <- NULL
  if (lrtest) {
    degrees <- K * (K + 1) / 2 - l
    if(identical(degrees, 0)) {
      warning(paste("The SVEC is just identified. No test possible."))
    } else {
      rSigma <- solve(A) %*% B %*% t(B) %*% t(solve(A))
      det1 <- det(rSigma)
      det2 <- det(Sigma)
      STATISTIC <- (log(det1) - log(det2)) * obs
      names(STATISTIC) <- "Chi^2"
      PARAMETER <- degrees
      names(PARAMETER) <- "df"
      PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
      METHOD <- "LR overidentification"
      LRover <- list(statistic = STATISTIC, parameter = PARAMETER, p.value = PVAL, method = METHOD, data.name = deparse(substitute(x)))
      class(LRover) <- "htest"
    }
  }
  BOOTVAL <- NULL
  SRse <- NULL
  LRse <- NULL
  if(boot){
    BOOTVAL <- .bootsvec(x = x, LRorig = LRorig, SRorig = SRorig, r = r, runs = runs, K = K, conv.crit = conv.crit, maxls = maxls, max.iter = max.iter)
    ##
    ## Calculating the standard deviations for parameters
    ##
    SRboot <- BOOTVAL[1:K2, ]
    SRse <- matrix(sqrt(apply((SRboot - c(SR))^2, 1, mean)), nrow = K, ncol = K)
    idxnull <- which(abs(SRse) < 0.1e-8, arr.ind = TRUE)
    SRse[idxnull] <- 0.0    
    LRboot <- BOOTVAL[-c(1:K2), ]
    LRse <- matrix(sqrt(apply((LRboot - c(LR))^2, 1, mean)), nrow = K, ncol = K)
    idxnull <- which(abs(LRse) < 0.1e-8, arr.ind = TRUE)
    LRse[idxnull] <- 0.0
    colnames(SRse) <- colnames(SR)
    rownames(SRse) <- rownames(SR)
    colnames(LRse) <- colnames(LR)
    rownames(LRse) <- rownames(LR)  
  }
  ##
  ## Setting near zero elements to zero
  ##
  idxnull <- which(abs(LR) < 0.1e-8, arr.ind = TRUE)
  LR[idxnull] <- 0.0
  idxnull <- which(abs(SR) < 0.1e-8, arr.ind = TRUE)
  SR[idxnull] <- 0.0
  ##
  ## Assembling svecest object
  ##
  result <- list(SR = SR, SRse = SRse, LR = LR, LRse = LRse, Sigma.U = Sigma.U * 100, Restrictions = c(nResC1, nResB), LRover = LRover, start = start, type = svartype, var = x, LRorig = LRorig, SRorig = SRorig, r = r, iter = iter, call = match.call())
  class(result) <- "svecest"
  return(result)
}
