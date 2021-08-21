toMlm<- function(x, ...) {
  UseMethod("toMlm")
}

toMlm.default <- function(x){
  lm(x$model)
}

toMlm.varest<-function(x){
  ix <- 1:x$K
  X <- x$datamat
  int <- "-1" #automatic intercept is removed by default
  xr <- x$restrictions
  if (x$type %in% c("const", "both")) {
    #if x contains a constant, remove it from data X but allow to be added by lm automatically
    #remove constant in datamat
    X <- X[, -grep("const", colnames(X))]
    int <- ""
    if (!is.null(xr)) {
      xr <- xr[, -grep("const", colnames(xr))]
    }
  }
  #construct formula
  left <- paste(names(X)[ix], collapse = ",")
  fo <- as.formula(paste("cbind(", left, ") ~ .", int))
  #apply lm
  res<-eval(substitute(lm(fo, X), list(fo = fo))) #code suggested by Gabor Groothendick
  if (!is.null(xr)) {
    for (i in ix) {
      foi <- as.formula(paste(names(X)[i], "~",
                              paste(colnames(X)[-ix][xr[i,] == 1], collapse = "+"),
                              int))
      mi <- eval(substitute(lm(fo, X), list(fo = foi)))
      #replace elements in the res object
      res$residuals[,i] <- mi$residuals
      res$effects[,i] <- mi$effects
      res$fitted.values[,i] <- mi$fitted.values
      #coefficients vectors are of different lengths, match them first
      #the code is bulky to avoid using other packages such as plyr::join
      rc <- data.frame(name = rownames(res$coefficients), rc = res$coefficients[,i])
      rc$id <- 1:nrow(rc)
      mc <- data.frame(name = names(mi$coefficients), mc = mi$coefficients)
      L <- merge(rc, mc, all.x = TRUE)
      L <- L[order(L$id),]
      L[is.na(L)] <- 0L
      res$coefficients[,i] <- as.numeric(L$mc)
    }
  }
  return(res)
}

coeftest.varest<-function(x, ...){
  coeftest(toMlm.varest(x), ...)
}

bread.varest<-function(x, ...){
  bread(toMlm.varest(x), ...)
}

vcov.varest<-function(object, ...){
  vcov(toMlm.varest(object), ...)
}

vcovHC.varest<-function(x, ...){
  vcovHC(toMlm.varest(x), ...)
}

estfun.varest<-function(x, ...){
  estfun(toMlm.varest(x), ...)
}
