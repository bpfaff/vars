###################################################
### chunk number 1: preliminaries
###################################################
library(vars)
library(urca)
library(xtable)


###################################################
### chunk number 2: data
###################################################
library(vars)
data(Canada)
layout(matrix(1:4, nrow = 2, ncol = 2))
plot.ts(Canada$e, main="Employment", ylab = "", xlab = "")
plot.ts(Canada$prod, main="Productivity", ylab = "", xlab = "")
plot.ts(Canada$rw, main="Real Wage", ylab = "", xlab = "")
plot.ts(Canada$U, main="Unemployment Rate", ylab = "", xlab = "")


###################################################
### chunk number 3: Canada
###################################################
layout(matrix(1:4, nrow = 2, ncol = 2))
plot.ts(Canada$e, main="Employment", ylab = "", xlab = "")
plot.ts(Canada$prod, main="Productivity", ylab = "", xlab = "")
plot.ts(Canada$rw, main="Real Wage", ylab = "", xlab = "")
plot.ts(Canada$U, main="Unemployment Rate", ylab = "", xlab = "")


###################################################
### chunk number 4: VAR1
###################################################
args(VAR)
args(VARselect)
VARselect(Canada, lag.max = 5, type = "const") 


###################################################
### chunk number 5: VAR2
###################################################
var.2c <- VAR(Canada, p = 2, type = "const")
names(var.2c)


###################################################
### chunk number 6: VAR3
###################################################
summary(var.2c)
plot(var.2c)


###################################################
### chunk number 7: VARsum.e
###################################################
xtable(summary(var.2c)[[1]], caption = "Regression result for employment equation", label = "var.2c.e")


###################################################
### chunk number 8: 
###################################################
layout(matrix(c(1, 1, 2, 2, 3, 4), nrow = 3, ncol = 2, byrow = TRUE))
plot.ts(var.2c$datamat[, 1], main = paste("Diagram of fit for", colnames(var.2c$datamat)[1], sep=" "), ylim = c(min(c(var.2c$datamat[, 1], var.2c$varresult[[1]]$fitted.values)), max(c(var.2c$datamat[, 1], var.2c$varresult[[1]]$fitted.values))), ylab = "", lty = 1)
lines(var.2c$varresult[[1]]$fitted.values, col = "blue", lty = 2)
plot.ts(var.2c$varresult[[1]]$residuals, main = "Residuals", ylab = "", lty = 1)
abline(h = 0, col = "red")
acf(var.2c$varresult[[1]]$residuals, main = "ACF Residuals", ylab = "")
pacf(var.2c$varresult[[1]]$residuals, main = "PACF Residuals", ylab = "")


###################################################
### chunk number 9: VARsum.prod
###################################################
xtable(summary(var.2c)[[2]], caption = "Regression result for productivity equation", label = "var.2c.prod")


###################################################
### chunk number 10: 
###################################################
layout(matrix(c(1, 1, 2, 2, 3, 4), nrow = 3, ncol = 2, byrow = TRUE))
plot.ts(var.2c$datamat[, 2], main = paste("Diagram of fit for", colnames(var.2c$datamat)[2], sep=" "), ylim = c(min(c(var.2c$datamat[, 2], var.2c$varresult[[2]]$fitted.values)), max(c(var.2c$datamat[, 2], var.2c$varresult[[2]]$fitted.values))), ylab = "", lty = 1)
lines(var.2c$varresult[[2]]$fitted.values, col = "blue", lty = 2)
plot.ts(var.2c$varresult[[2]]$residuals, main = "Residuals", ylab = "", lty = 1)
abline(h = 0, col = "red")
acf(var.2c$varresult[[2]]$residuals, main = "ACF Residuals", ylab = "")
pacf(var.2c$varresult[[2]]$residuals, main = "PACF Residuals", ylab = "")


###################################################
### chunk number 11: VARsum.rw
###################################################
xtable(summary(var.2c)[[3]], caption = "Regression result for real wage equation", label = "var.2c.rw")


###################################################
### chunk number 12: 
###################################################
layout(matrix(c(1, 1, 2, 2, 3, 4), nrow = 3, ncol = 2, byrow = TRUE))
plot.ts(var.2c$datamat[, 3], main = paste("Diagram of fit for", colnames(var.2c$datamat)[3], sep=" "), ylim = c(min(c(var.2c$datamat[, 3], var.2c$varresult[[3]]$fitted.values)), max(c(var.2c$datamat[, 3], var.2c$varresult[[3]]$fitted.values))), ylab = "", lty = 1)
lines(var.2c$varresult[[3]]$fitted.values, col = "blue", lty = 2)
plot.ts(var.2c$varresult[[3]]$residuals, main = "Residuals", ylab = "", lty = 1)
abline(h = 0, col = "red")
acf(var.2c$varresult[[3]]$residuals, main = "ACF Residuals", ylab = "")
pacf(var.2c$varresult[[3]]$residuals, main = "PACF Residuals", ylab = "")


###################################################
### chunk number 13: VARsum.U
###################################################
xtable(summary(var.2c)[[4]], caption = "Regression result for unemployment equation", label = "var.2c.U")


###################################################
### chunk number 14: 
###################################################
layout(matrix(c(1, 1, 2, 2, 3, 4), nrow = 3, ncol = 2, byrow = TRUE))
plot.ts(var.2c$datamat[, 4], main = paste("Diagram of fit for", colnames(var.2c$datamat)[4], sep=" "), ylim = c(min(c(var.2c$datamat[, 4], var.2c$varresult[[4]]$fitted.values)), max(c(var.2c$datamat[, 4], var.2c$varresult[[4]]$fitted.values))), ylab = "", lty = 1)
lines(var.2c$varresult[[4]]$fitted.values, col = "blue", lty = 2)
plot.ts(var.2c$varresult[[4]]$residuals, main = "Residuals", ylab = "", lty = 1)
abline(h = 0, col = "red")
acf(var.2c$varresult[[4]]$residuals, main = "ACF Residuals", ylab = "")
pacf(var.2c$varresult[[4]]$residuals, main = "PACF Residuals", ylab = "")


###################################################
### chunk number 15: VAR.stable
###################################################
roots(var.2c)


###################################################
### chunk number 16: restrict1
###################################################
args(restrict)
## Restrictions by significance
var2c.ser <- restrict(var.2c, method = "ser", thresh = 2.0)
var2c.ser$restrictions
B(var2c.ser)
## Restrictions set manually for third and fourth coefficient
## in first equation
res <- matrix(rep(1, 36), nrow = 4, ncol = 9)
res[1, 3] <- 0
res[1, 4] <- 0
var2c.man <- restrict(var.2c, method = "manual", resmat = res)
var2c.man$restrictions
B(var2c.man)


###################################################
### chunk number 17: diag0
###################################################
args(arch)
var2c.arch <- arch(var.2c)
names(var2c.arch)
var2c.arch


###################################################
### chunk number 18: diag1
###################################################
var2c.norm <- normality(var.2c, multivariate.only = TRUE)
names(var2c.norm)
var2c.norm
plot(var2c.norm)


###################################################
### chunk number 19: diag2
###################################################
var2c.pt.asy <- serial(var.2c, lags.pt = 16, type = "PT.asymptotic")
var2c.pt.asy
var2c.pt.adj <- serial(var.2c, lags.pt = 16, type = "PT.adjusted")
var2c.pt.adj
plot(var2c.pt.asy)


###################################################
### chunk number 20: diag3
###################################################
var2c.BG <- serial(var.2c, lags.pt = 16, type = "BG")
var2c.BG
var2c.ES <- serial(var.2c, lags.pt = 16, type = "ES")
var2c.ES


###################################################
### chunk number 21: diag4
###################################################
args(stability)
var2c.stab <- stability(var.2c, type = "OLS-CUSUM")
names(var2c.stab)
plot(var2c.stab)


###################################################
### chunk number 22: 
###################################################
plot(var2c.stab$stability$e)


###################################################
### chunk number 23: 
###################################################
plot(var2c.stab$stability$prod)


###################################################
### chunk number 24: 
###################################################
plot(var2c.stab$stability$rw)


###################################################
### chunk number 25: 
###################################################
plot(var2c.stab$stability$U)


###################################################
### chunk number 26: cause1
###################################################
args(causality)


###################################################
### chunk number 27: cause2
###################################################
causality(var.2c, cause = c("rw", "prod"))


###################################################
### chunk number 28: predict1
###################################################
var.f10 <- predict(var.2c, n.ahead = 10, ci = 0.95)
names(var.f10)
class(var.f10)
plot(var.f10)
fanchart(var.f10)


###################################################
### chunk number 29: fanchart1
###################################################
args(fanchart)


###################################################
### chunk number 30: 
###################################################
smpl <- nrow(var.f10$endog)
ynames <- colnames(var.f10$endog)
fcsty <- c(rep(NA, smpl - 1), var.f10$endog[smpl, 1], var.f10$fcst[[1]][, 1])
fcstl <- c(rep(NA, smpl - 1), var.f10$endog[smpl, 1], var.f10$fcst[[1]][, 2])
fcstu <- c(rep(NA, smpl - 1), var.f10$endog[smpl, 1], var.f10$fcst[[1]][, 3])
smply <- c(var.f10$endog[, 1], rep(NA, length(var.f10$fcst[[1]][, 1])))
min.y <- min(na.omit(c(fcsty, fcstl, fcstu, smply)))
max.y <- max(na.omit(c(fcsty, fcstl, fcstu, smply)))               
plot.ts(fcsty, ylab = "", xlab = "", ylim = c(min.y, max.y), main = paste("Forecast of series", ynames[1]), col = "blue", lty = 2)
lines(smply, col = "black", lty = 1)
lines(fcstl, col = "red", lty = 3)
lines(fcstu, col = "red", lty = 3)
abline(v = smpl, col = "grey", lty = 4)


###################################################
### chunk number 31: 
###################################################
colors <- gray(sqrt(seq(from = 0.05, to = 1, length = 9)))
cis <- seq(0.1, 0.9, by = 0.1)
n.regions <- length(cis)
n.ahead <- nrow(var.f10$fcst[[1]])
e.sample <- nrow(var.f10$endog)
endog <- var.f10$endog
fcst <- NULL 
for(j in 1:n.regions){
fcst[[j]] <- predict(var.f10$model, n.ahead = n.ahead, ci = cis[j])$fcst
}
xx <- seq(e.sample, length.out = n.ahead + 1)
xx <- c(xx, rev(xx))
ymax <- max(c(fcst[[n.regions]][4][[1]][, 3]), endog[, 4])
ymin <- min(c(fcst[[n.regions]][4][[1]][, 2]), endog[, 4])
yy1 <- c(endog[e.sample, 4], fcst[[1]][4][[1]][, 2], rev(c(endog[e.sample, 4], fcst[[1]][4][[1]][, 3])))
plot.ts(c(endog[, 4], rep(NA, n.ahead)), main = paste("Fan chart for variable", colnames(endog)[4]), xlab = "", ylab = "", ylim = c(ymin, ymax))
polygon(xx, yy1, col = colors[1], border = colors[1])
if(n.regions > 1){
for(l in 2:n.regions){
   yyu <- c(endog[e.sample, 4], fcst[[l]][4][[1]][, 3], rev(c(endog[e.sample, 4], fcst[[l-1]][4][[1]][, 3])))
   yyl <- c(endog[e.sample, 4], fcst[[l-1]][4][[1]][, 2], rev(c(endog[e.sample, 4], fcst[[l]][4][[1]][, 2])))
   polygon(xx, yyu, col = colors[l], border = colors[l])
   polygon(xx, yyl, col = colors[l], border = colors[l])
  }
}


###################################################
### chunk number 32: irf1
###################################################
args(irf)
Canada2 <- Canada[, c(3, 1, 4, 2)]
names(Canada2)
var.2c.alt <- VAR(Canada2, p = 2, type = "const")
irf.rw.eU <- irf(var.2c.alt, impulse = "rw", response = c("e", "U"), boot = TRUE) 
names(irf.rw.eU)
plot(irf.rw.eU)


###################################################
### chunk number 33: 
###################################################
plot(irf.rw.eU)


###################################################
### chunk number 34: fevd1
###################################################
args(fevd)


###################################################
### chunk number 35: fevd2
###################################################
var2c.fevd <- fevd(var.2c, n.ahead = 5)
class(var2c.fevd)
names(var2c.fevd)
var2c.fevd$e


###################################################
### chunk number 36: fevd3
###################################################
barplot(t(var2c.fevd[[1]]), main = paste("FEVD for", names(var2c.fevd)[1]), col = palette()[1 : 4], ylab = "Percentage", xlab = "Horizon", names.arg = paste(1 : nrow(var2c.fevd[[1]])), ylim = c(0, 1.2))
legend("top", legend = names(var2c.fevd), fill = palette()[1 : 4], ncol = 4)


###################################################
### chunk number 37: SVAR1
###################################################
args(SVAR)
amat <- diag(4)
diag(amat) <- NA
amat[1, 2] <- NA
amat[1, 3] <- NA
amat[3, 2] <- NA
amat[4, 1] <- NA
amat


###################################################
### chunk number 38: SVAR2
###################################################
args(optim)
svar2c.A <- SVAR(var.2c, estmethod = "logLik", Amat = amat, Bmat = NULL, 
   hessian = TRUE, method = "BFGS") 
svar2c.A


###################################################
### chunk number 39: SVAR3
###################################################
class(svar2c.A)
names(svar2c.A)


###################################################
### chunk number 40: svar-bq
###################################################
BQ(var.2c)


###################################################
### chunk number 41: svar-irf1
###################################################
svar2cA.ira <- irf(svar2c.A, impulse = "rw", response = c("e", "U"), boot = FALSE)
svar2cA.ira


###################################################
### chunk number 42: fevd-svar
###################################################
svar2cA.fevd <- fevd(svar2c.A , n.ahead = 8)
plot(svar2cA.fevd)


###################################################
### chunk number 43: fevd-svar-fig
###################################################
barplot(t(svar2cA.fevd[[3]]), main = paste("FEVD for", names(svar2cA.fevd)[3]), col = palette()[1 : 4], ylab = "Percentage", xlab = "Horizon", names.arg = paste(1 : nrow(svar2cA.fevd[[3]])), ylim = c(0, 1.2))
legend("top", legend = names(svar2cA.fevd), fill = palette()[1 : 4], ncol = 4)


###################################################
### chunk number 44: vecm
###################################################
library(urca)
data(finland)
sjf <- finland
sjf.vecm <- ca.jo(sjf, constant = FALSE, type = "eigen", K = 2,
spec = "longrun", season = 4, ctable = "A2")
summary(sjf.vecm)


###################################################
### chunk number 45: vec2var
###################################################
args(vec2var)
sjf.var <- vec2var(sjf.vecm, r = 2)
sjf.var


###################################################
### chunk number 46: vec2var2
###################################################
names(sjf.var)
class(sjf.var)
methods(class = "vec2var")


