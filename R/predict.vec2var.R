"predict.vec2var" <-
function(object, ..., n.ahead = 10, ci = 0.95, dumvar = NULL){
  n.ahead <- as.integer(n.ahead)
  K <- object$K
  p <- object$p
  obs <- object$obs
  data.all <- object$datamat
  ynames <- colnames(object$y)
  Z <- object$datamat[, -c(1 : K)]
  B <- object$deterministic
  for(i in 1:object$p){
    B <- cbind(B, object$A[[i]])
  }
  ## Deterministic and lagged y's
  ## Retrieval of A in matrix (whole)
  Zdet <- matrix(rep(1, n.ahead), nrow = n.ahead, ncol = 1)
  rownames(Zdet) <- seq(nrow(data.all) + 1, length = n.ahead)
  if(eval(object$vecm@ecdet) == "trend"){
    trendf <- seq(obs + p, length = n.ahead)
    Zdet <- cbind(Zdet, trendf)
  }
  if(!is.null(eval(object$vecm@season))){
    season <- eval(object$vecm@season)
    seas.names <- paste("sd", 1:(season-1), sep = "")
    cycle <- tail(data.all[, seas.names], season)
    seasonal <- matrix(cycle, nrow = season, ncol = season - 1)
    if(nrow(seasonal) >= n.ahead){
      seasonal <- matrix(cycle[1:n.ahead, ], nrow = n.ahead, ncol = season -1 )
    } else {
      while(nrow(seasonal) < n.ahead){
        seasonal <- rbind(seasonal, cycle)
      }
      seasonal <- seasonal[1:n.ahead, ]
    }
    rownames(seasonal) <- seq(nrow(data.all) + 1, length = n.ahead)
    Zdet <- cbind(Zdet, seasonal)
  }
  if(!is.null(eval(object$vecm@dumvar))){
    if(is.null(dumvar)){
      stop(paste("\nPlease, provide a matrix x for argument 'dumvar' with", n.ahead, "rows.\n", sep = " "))
    }
    if(!identical(nrow(dumvar), n.ahead)){
      stop("\nNumber of rows of 'dumvar' is not equal to 'n.ahead'.\n")
    }
    testsum <- sum((colnames(dumvar) %in% colnames(B)))
    if(!(testsum == ncol(dumvar))){
      stop("\nColumn names of 'dumvar' do not match with column names in 'object$datamat'.\n")
    }
    Zdet <- cbind(Zdet, dumvar)
  }
  exogen.cols <- which(colnames(data.all) %in% colnames(object$deterministic))
  Zy <- data.all[, -exogen.cols] 
  yse <- matrix(NA, nrow = n.ahead, ncol = K)
  sig.y <- .fecovvec2var(x = object, n.ahead = n.ahead)
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
    Z <- c(Zdet[i, ], lasty)
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

