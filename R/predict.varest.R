"predict.varest" <-
function(object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL){
  K <- object$K
  p <- object$p
  obs <- object$obs
  type <- object$type
  data.all <- object$datamat
  ynames <- colnames(object$y)
  n.ahead <- as.integer(n.ahead)
  Z <- object$datamat[, -c(1 : K)]
  B <- Bcoef(object)
  ##
  ## Deterministic and lagged y's
  ## Retrieval of A in matrix (whole)
  ## Deterministic variables in Zdet
  ##
  if(type == "const"){
      Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
      colnames(Zdet) <- "const"
    }else if(type == "trend"){
      trdstart <- nrow(Z) + 1 + p
      Zdet <- matrix(seq(trdstart, length = n.ahead), nrow = n.ahead, ncol = 1)
      colnames(Zdet) <- "trend"
    }else if(type == "both"){
      trdstart <- nrow(Z) + 1 + p
      Zdet <- matrix(c(rep(1, n.ahead), seq(trdstart, length = n.ahead)), nrow = n.ahead, ncol = 2)
      colnames(Zdet) <- c("const", "trend")
    }else if(type == "none"){
      Zdet <- NULL
    }
  ## Include seasonal if applicable
  if(!is.null(eval(object$call$season))){
    season <- eval(object$call$season)
    seas.names <- paste("sd", 1:(season-1), sep = "")
    cycle <- tail(data.all[, seas.names], season)
    seasonal <- as.matrix(cycle, nrow = season, ncol = season - 1)
    if(nrow(seasonal) >= n.ahead){
      seasonal <- as.matrix(cycle[1:n.ahead, ], nrow = n.ahead, ncol = season -1 )
    } else {
      while(nrow(seasonal) < n.ahead){
        seasonal <- rbind(seasonal, cycle)
      }
      seasonal <- seasonal[1:n.ahead, ]
    }
    rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
    if(!is.null(Zdet)){
      Zdet <- as.matrix(cbind(Zdet, seasonal))
    } else {
      Zdet <- as.matrix(seasonal)
    }      
  }
  ## Include exogenous variables if applicable
  if(!is.null(eval(object$call$exogen))){
    if(is.null(dumvar)){
      stop("\nNo matrix for dumvar supplied, but object varest contains exogenous variables.\n")
    }
    if(!all(colnames(dumvar) %in% colnames(data.all))){
      stop("\nColumn names of dumvar do not coincide with exogen.\n")
    }
    if(!identical(nrow(dumvar), n.ahead)){
      stop("\nRow number of dumvar is unequal to n.ahead.\n")
    }
    if(!is.null(Zdet)){
      Zdet <- as.matrix(cbind(Zdet, dumvar))
    } else {
      Zdet <- as.matrix(dumvar)
    }      
  }
  ## Retrieving predetermined y variables
  Zy <- as.matrix(object$datamat[, 1:(K * (p + 1))]) 
  yse <- matrix(NA, nrow = n.ahead, ncol = K)
  sig.y <- .fecov(x = object, n.ahead = n.ahead)
  for(i in 1 : n.ahead){
    yse[i, ] <- sqrt(diag(sig.y[, , i]))
  }
  yse <- -1 * qnorm((1 - ci) / 2) * yse
  colnames(yse) <- paste(ci, "of", ynames)
  ## forecast recursion
  forecast <- matrix(NA, ncol = K, nrow = n.ahead)
  lasty <- c(Zy[nrow(Zy), ])
  for(i in 1 : n.ahead){
    lasty <- lasty[1 : (K * p)]
    Z <- c(lasty, Zdet[i, ])
    forecast[i, ] <- B %*% Z
    temp <- forecast[i, ]
    lasty <- c(temp, lasty)
  }
  colnames(forecast) <- paste(ynames, ".fcst", sep="")
  lower <- forecast - yse
  colnames(lower) <- paste(ynames, ".lower", sep="")
  upper <- forecast + yse
  colnames(upper) <- paste(ynames, ".upper", sep="")
  forecasts <- list()
  for(i in 1 : K){
    forecasts[[i]] <- cbind(forecast[, i], lower[, i], upper[, i], yse[, i])
    colnames(forecasts[[i]]) <- c("fcst", "lower", "upper", "CI")
  }
  names(forecasts) <- ynames
  result <- list(fcst = forecasts, endog = object$y, model = object, exo.fcst = dumvar) 
  class(result) <- "varprd"
  return(result)
}
