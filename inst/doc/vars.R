### R code from vignette source 'vars.Rnw'

###################################################
### code chunk number 1: source
###################################################
###################################################
### Preliminaries
###################################################
library("vars")
data("Canada")
summary(Canada)
plot(Canada, nc = 2, xlab = "")


###################################################
### ADF - Tests
###################################################
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf2 <-summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))
adf3 <-summary(ur.df(Canada[, "e"], type = "trend", lags = 2))
adf4 <-summary(ur.df(diff(Canada[, "e"]), type = "drift", lags = 1))
adf5 <-summary(ur.df(Canada[, "U"], type = "drift", lags = 1))
adf6 <-summary(ur.df(diff(Canada[, "U"]), type = "none", lags = 0))
adf7 <-summary(ur.df(Canada[, "rw"], type = "trend", lags = 4))
adf8 <-summary(ur.df(diff(Canada[, "rw"]), type = "drift", lags = 3))
adf9 <-summary(ur.df(diff(Canada[, "rw"]), type = "drift", lags = 0))


###################################################
### Lag-order selection
###################################################
VARselect(Canada, lag.max = 8, type = "both")


###################################################
### VAR(1)
###################################################
Canada <- Canada[, c("prod", "e", "U", "rw")]
p1ct <- VAR(Canada, p = 1, type = "both")
p1ct
summary(p1ct, equation = "e")
plot(p1ct, names = "e")


###################################################
### VAR(2) & VAR(3)
###################################################
p2ct <- VAR(Canada, p = 2, type = "both")
p3ct <- VAR(Canada, p = 3, type = "both")


###################################################
### Diagnostic Tests 1
###################################################
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial
norm1 <-normality.test(p1ct)
norm1$jb.mul
arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul
plot(arch1, names = "e")
plot(stability(p1ct), nc = 2)


###################################################
### Diagnostic Tests 2
###################################################
## Serial
ser31 <- serial.test(p3ct, lags.pt = 16, type = "PT.asymptotic")$serial
ser21 <- serial.test(p2ct, lags.pt = 16, type = "PT.asymptotic")$serial
ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")$serial
ser32 <- serial.test(p3ct, lags.pt = 16, type = "PT.adjusted")$serial
ser22 <- serial.test(p2ct, lags.pt = 16, type = "PT.adjusted")$serial
ser12 <- serial.test(p1ct, lags.pt = 16, type = "PT.adjusted")$serial
## JB
norm3 <- normality.test(p3ct)$jb.mul$JB
norm2 <-normality.test(p2ct)$jb.mul$JB
norm1 <-normality.test(p1ct)$jb.mul$JB
## ARCH
arch3 <- arch.test(p3ct, lags.multi = 5)$arch.mul
arch2 <- arch.test(p2ct, lags.multi = 5)$arch.mul
arch1 <- arch.test(p1ct, lags.multi = 5)$arch.mul


###################################################
### VECM
###################################################
vecm.p3 <- summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3, spec = "transitory"))
vecm.p2 <- summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2, spec = "transitory"))


###################################################
### VECM r = 1
###################################################
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace", ecdet = "trend", K = 3, spec = "transitory") 
vecm.r1 <- cajorls(vecm, r = 1)
##
## Calculation of t-values for alpha and beta
##
alpha <- coef(vecm.r1$rlm)[1, ]
names(alpha) <- c("rw", "prod", "e", "U")
alpha
beta <- vecm.r1$beta
beta
resids <- resid(vecm.r1$rlm)
N <- nrow(resids)
sigma <- crossprod(resids) / N
## t-stats for alpha (calculated by hand)
alpha.se <- sqrt(solve(crossprod(cbind(vecm@ZK %*% beta, vecm@Z1)))[1, 1] * diag(sigma))
names(alpha.se) <-  c("rw", "prod", "e", "U")
alpha.t <- alpha / alpha.se
alpha.t
## Differ slightly from coef(summary(vecm.r1$rlm))
## due to degrees of freedom adjustment 
coef(summary(vecm.r1$rlm))
## t-stats for beta
beta.se <- sqrt(diag(kronecker(solve(crossprod(vecm@RK[, -1])),
                               solve(t(alpha) %*% solve(sigma) %*% alpha))))
beta.t <- c(NA, beta[-1] / beta.se)
names(beta.t) <- rownames(vecm.r1$beta)
beta.t


###################################################
### SVEC
###################################################
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace", 
              ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, 
             boot = TRUE, runs = 100)
summary(svec)


###################################################
### SR-table
###################################################
SR <- round(svec$SR, 2)
SRt <- round(svec$SR / svec$SRse, 2)


###################################################
### LR-table
###################################################
LR <- round(svec$LR, 2)
LRt <- round(svec$LR / svec$LRse, 2)


###################################################
### Over-identification
###################################################
LR[3, 3] <- 0
svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.oi$LRover


###################################################
### SVEC - IRF
###################################################
svec.irf <- irf(svec, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf)


###################################################
### SVEC - FEVD
###################################################
fevd.U <- fevd(svec, n.ahead = 48)$U


