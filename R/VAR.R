"VAR" <- 
function (y, p = 1, type = c("const", "trend", "both", "none"), 
    season = NULL, exogen = NULL, lag.max = NULL, ic = c("AIC", "HQ", "SC", "FPE")) 
{
  y <- as.matrix(y)
    if (any(is.na(y))) 
        stop("\nNAs in y.\n")
    if (ncol(y) < 2) 
        stop("The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.\n")
    if (is.null(colnames(y))) {
        colnames(y) <- paste("y", 1:ncol(y), sep = "")
        warning(paste("No column names supplied in y, using:", 
            paste(colnames(y), collapse = ", "), ", instead.\n"))
    }
    colnames(y) <- make.names(colnames(y))
    y.orig <- y
    type <- match.arg(type)
    obs <- dim(y)[1]
    K <- dim(y)[2]
    if(!is.null(lag.max)){
      lag.max <- abs(as.integer(lag.max))
      ic <- paste(match.arg(ic), "(n)", sep = "")
      p <- VARselect(y, lag.max = lag.max, type = type, season = season, exogen = exogen)$selection[ic]
    }
    sample <- obs - p
    ylags <- embed(y, dimension = p + 1)[, -(1:K)]
    temp1 <- NULL
    for (i in 1:p) {
        temp <- paste(colnames(y), ".l", i, sep = "")
        temp1 <- c(temp1, temp)
    }
    colnames(ylags) <- temp1
    yend <- y[-c(1:p), ]
    if (type == "const") {
        rhs <- cbind(ylags, rep(1, sample))
        colnames(rhs) <- c(colnames(ylags), "const")
    }
    else if (type == "trend") {
        rhs <- cbind(ylags, seq(p + 1, length = sample))
        colnames(rhs) <- c(colnames(ylags), "trend")
    }
    else if (type == "both") {
        rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
        colnames(rhs) <- c(colnames(ylags), "const", "trend")
    }
    else if (type == "none") {
        rhs <- ylags
        colnames(rhs) <- colnames(ylags)
    }
    if (!(is.null(season))) {
        season <- abs(as.integer(season))
        dum <- (diag(season) - 1/season)[, -season]
        dums <- dum
        while (nrow(dums) < obs) {
            dums <- rbind(dums, dum)
        }
        dums <- dums[1:obs, ]
        colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
        rhs <- cbind(rhs, dums[-c(1:p), ])
    }
    if (!(is.null(exogen))) {
        exogen <- as.matrix(exogen)
        if (!identical(nrow(exogen), nrow(y))) {
            stop("\nDifferent row size of y and exogen.\n")
        }
        if (is.null(colnames(exogen))) {
            colnames(exogen) <- paste("exo", 1:ncol(exogen), 
                sep = "")
            warning(paste("No column names supplied in exogen, using:", 
                paste(colnames(exogen), collapse = ", "), ", instead.\n"))
        }
        colnames(exogen) <- make.names(colnames(exogen))
        tmp <- colnames(rhs)
        rhs <- cbind(rhs, exogen[-c(1:p), ])
        colnames(rhs) <- c(tmp, colnames(exogen))
    }
    datamat <- as.data.frame(rhs)
    colnames(datamat) <- colnames(rhs)
    equation <- list()
    for (i in 1:K) {
        y <- yend[, i]
        equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datamat)
        if(any(c("const", "both") %in% type)){
         attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
        }
    }
  call <- match.call()
  if("season" %in% names(call)) call$season <- eval(season)
    result <- list(varresult = equation, datamat = data.frame(cbind(yend, 
        rhs)), y = y.orig, type = type, p = p, K = K, obs = sample, 
        totobs = sample + p, restrictions = NULL, call = call)
    class(result) <- "varest"
    return(result)
}
