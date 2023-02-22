#PACKAGES ##
install.packages("tsDyn")
library(tsDyn)
install.packages("cointReg")
library(cointReg)

TVECM.HStest

# we test monthly first: 

hh_wti <- cbind(diff(hh_monthly_log), diff(wti_monthly_log))
hh_brent <- cbind(diff(hh_monthly_log), diff(brent_monthly_log))
hh_ttf <- cbind(diff(hh_monthly_log), diff(ttf_monthly_log))
hh_jkm <- cbind(diff(hh_monthly_log), diff(jkm_monthly_log))
hh_nbp <- cbind(diff(hh_monthly_log), diff(nbp_monthly_log))


hh#TVECM.HStest(data, lag=1, ngridTh=300, trim=0.05, 
 #            nboot=100, fixed.beta=NULL,  intercept=TRUE, 
#             boot.type=c("FixedReg", "ResBoot"), 
#             hpc=c("none", "foreach"))


#lag	: Number of lags to include in each regime

#ngridTh	: Number of threshold grid points to evaluate the optimal threshold.

# trim: Trimming parameter indicating the minimal percentage of observations in each regime

#nboot	: Number of bootstrap replications

#fixed.beta: Numeric. User pre-specified cointegrating value, as in VECM (i.e. cointegrating vector will be c(1, -beta.fixed)). When NULL (default), the value is estimated from the linear VECM.

# intercept: Logical. Whether an intercept has to be included in the VECM

result_1 <- TVECM.HStest(hh_wti[,c(1:2)])
result_2 <- TVECM.HStest(hh_brent[,c(1:2)])

# adjust the time length of each of these

result_3 <- TVECM.HStest(hh_ttf[,c(1:2)])
result_4 <- TVECM.HStest(hh_jkm[,c(1:2)])

result_5 <- TVECM.HStest(hh_nbp[,c(1:2)])
hh_nbp <- hh_nbp[-309,]


# null of no cointegration

TVECM.SeoTest(hh_nbp[,c(1:2)], lag=1, beta=1, nboot=1000)
KapShinTest(hh_monthly)


# DOLS ################################################

#Tests for cointegration with structural breaks based on subsamples
cointRegD(x, y, deter, kernel = c("ba", "pa", "qs", "tr"),
          bandwidth = c("and", "nw"), n.lead = NULL, n.lag = NULL,
          kmax = c("k4", "k12"), info.crit = c("AIC", "BIC"), demeaning = FALSE,
          check = TRUE, ...)
length()

y <- diff(hh_monthly_log)
x <- diff(wti_monthly_log)
n<- length(hh_monthly_log)
deter = matrix(cbind(level = rep(1,n)))


DOLS_result <- cointRegD(x, y, deter, kmax = c("k4"))



cointRegD(wti_monthly_log, hh_monthly_log, deter, kmax = c("k12"))
summary(lm(y~x))

library(aod)

# we can add in an intercept -- though we want the intercept to be 0: 


wald.test(Sigma = DOLS_result$varmat, b = DOLS_result$beta, Terms = 1:2)



# now the window

