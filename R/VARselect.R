"VARselect" <- function(y,
                        lag.max = 10,
                        lag.restrict = 0,
                        type = c("const", "trend", "both", "none"),
                        season = NULL,
                        exogen = NULL)
{
    y <- as.matrix(y)
    if (any(is.na(y)))
        stop("\nNAs in y.\n")
    colnames(y) <- make.names(colnames(y))
    K <- ncol(y)
    lag.max <- abs(as.integer(lag.max))
    #VL: check that lag.restrict is an N0 number below lag.max [0,1,...,lag.max)
    lag.restrict <- abs(as.integer(lag.restrict))
    if (lag.restrict >= lag.max) {
      warning("lag.restrict >= lag.max. Using lag.restrict = 0 instead.")
      lag.restrict <- 0
    }
    type <- match.arg(type)
    lag <- abs(as.integer(lag.max + 1))
    ylagged <- embed(y, lag)[, -c(1:K)]
    yendog <- y[-c(1:lag.max), ]
    sample <- nrow(ylagged)
    rhs <- switch(type, const = rep(1, sample), trend = seq(lag.max + 1,
        length = sample), both = cbind(rep(1, sample), seq(lag.max + 1, length = sample)), none = NULL)
    if (!(is.null(season))) {
        season <- abs(as.integer(season))
        dum <- (diag(season) - 1/season)[, -season]
        dums <- dum
        while (nrow(dums) < sample) {
            dums <- rbind(dums, dum)
        }
        dums <- dums[1:sample, ]
        rhs <- cbind(rhs, dums)
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
        rhs <- cbind(rhs, exogen[-c(1:lag.max), ])
    }
    idx <- seq(K, K * lag.max, K)
    if(!is.null(rhs)){
      detint <- ncol(as.matrix(rhs))
    } else {
      detint <- 0
    }
    criteria <- matrix(NA, nrow = 4, ncol = lag.max - lag.restrict)
    rownames(criteria) <- c("AIC(n)", "HQ(n)", "SC(n)", "FPE(n)")
    colnames(criteria) <- (lag.restrict + 1):lag.max
    ii <- 1
    for (i in (lag.restrict + 1):lag.max) {
      if (lag.restrict == 0) {
        ys.lagged <- cbind(ylagged[, c(1:idx[i])], rhs)
        nstar <- ncol(ys.lagged)
        resids <- lm.fit(x=ys.lagged, y=yendog)$residuals
      } else {
        resids <- numeric()
        for (k in 1:K) {
          #VL: restrict to 0 coeffs 1,...,lag.restrict for all variables except k-th
          tmp <- rep(TRUE, K)
          tmp[k] <- FALSE #do not restrict for the current variable
          irestr <- which(rep(tmp, lag.restrict))
          ys.lagged <- cbind(ylagged[, c(1:idx[i])[-irestr]], rhs)
          resids <- cbind(resids, lm.fit(x = ys.lagged, y = yendog[,k])$residuals)
        }
        nstar <- ncol(ys.lagged)
      }
        sigma.det <- det(crossprod(resids)/sample)
        criteria[1, ii] <- log(sigma.det) + (2/sample) * (i * K^2 + K * detint)
        criteria[2, ii] <- log(sigma.det) + (2 * log(log(sample))/sample) * (i * K^2 + K * detint)
        criteria[3, ii] <- log(sigma.det) + (log(sample)/sample) * (i * K^2 + K * detint)
        criteria[4, ii] <- ((sample + nstar)/(sample - nstar))^K * sigma.det
        ii <- ii + 1
    }
    order <- apply(criteria, 1, which.min) + lag.restrict
    list(selection = order, criteria = criteria)
}
