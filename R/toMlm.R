toMlm<- function(x, ...) {
  UseMethod("toMlm")
}

toMlm.default <- function(x){
  lm(x$model)
}

toMlm.varest<-function(x){
  ix <- 1:x$K
  X<-x$datamat
  type<-x$type
  is.const<-type%in%c("const", "both")
  #remove constant in datamat
  if(is.const) X<-X[, -grep("const", colnames(X))]
  #construct formula
  left <- paste(names(X)[ix], collapse = ",")
  if(is.const) {
    fo <- as.formula(paste("cbind(", left, ") ~ ."))
  } else {
    fo <- as.formula(paste("cbind(", left, ") ~ .-1")) #remove automatical constant
  }
  #apply lm
  res<-eval(substitute(lm(fo, X), list(fo = fo))) #code suggested by Gabor Groothendick
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