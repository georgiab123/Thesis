#PACKAGES ##
install.packages("tsDyn")
library(tsDyn)
install.packages("cointReg")
library(cointReg)

TVECM.HStest

# we test monthly first: 

#hTVECM.HStest(data, lag=1, ngridTh=300, trim=0.05, 
 #            nboot=100, fixed.beta=NULL,  intercept=TRUE, 
#             boot.type=c("FixedReg", "ResBoot"), 
#             hpc=c("none", "foreach"))


#lag	: Number of lags to include in each regime

#ngridTh	: Number of threshold grid points to evaluate the optimal threshold.

# trim: Trimming parameter indicating the minimal percentage of observations in each regime

#nboot	: Number of bootstrap replications

#fixed.beta: Numeric. User pre-specified cointegrating value, as in VECM (i.e. cointegrating vector will be c(1, -beta.fixed)). When NULL (default), the value is estimated from the linear VECM.

# intercept: Logical. Whether an intercept has to be included in the VECM

# GREGORY HANSEN SUBSAMPLES, DAILY DATA

# BRENT 0th SEGMENT
y <- window(hh_daily_log, start = time(hh_daily_log)[1] ,  end = time(hh_daily_log)[4430])   
x <- window(brent_daily_log, start = time(hh_daily_log)[1], end = time(hh_daily_log)[4430]) 
hh_brent_0_gh_data <- cbind(y,x)
hh_brent_0_gh <- TVECM.HStest(hh_brent_0_gh[,c(1:2)])
hh_brent_0_gh
summary(hh_brent_0_gh)
plot(hh_brent_0_gh)
tvecm_brent_hh_0 <- TVECM(hh_brent_0_gh_data,  nthresh = 2, common = c("All"))
tvecm_brent_hh_0_res <- residuals(tvec)

# BRENT 1st SEGMENT 
y <- window(hh_daily_log, start = time(hh_daily_log)[4430] ,  end = time(hh_daily_log)[9425])   
x <- window(brent_daily_log, start = time(hh_daily_log)[4430], end = time(hh_daily_log)[9425]) 
hh_brent_1_gh <- cbind(y,x)
hh_brent_1_gh <- TVECM.HStest(hh_brent_1_gh[,c(1:2)])
hh_brent_1_gh
summary(hh_brent_1_gh)

# NBP 0th segment

y <- window(hh_daily_log, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[4959])   
x <- window(nbp_daily_log, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[4959]) 
hh_nbp_0_gh <- cbind(y,x)
hh_nbp_0_gh <- TVECM.HStest(hh_nbp_0_gh[,c(1:2)])
hh_nbp_0_gh
summary(hh_nbp_0_gh)

# NBP 1st SEGMENT 
y <- window(hh_daily_log, start =  time(nbp_daily_log)[4959] ,  end = time(nbp_daily_log)[length(nbp_daily_log)])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[4959], end = time(nbp_daily_log)[length(nbp_daily_log)]) 
hh_nbp_1_gh <- cbind(y,x)
hh_nbp_1_gh <- TVECM.HStest(hh_nbp_1_gh[,c(1:2)])
dev.off()
tvec <- TVECM(hh_nbp_1_gh[,c(1:2)], nthresh=2,lag=1, ngridBeta=20, ngridTh=30, plot=TRUE,trim=0.05, common="All")
print(tvec)
summary(tvec)
toLatex(tvec)





rresult_1 <- TVECM.HStest(hh_wti[,c(1:2)])
result_1
result_2 <- TVECM.HStest(hh_brent[,c(1:2)])
result_2

# adjust the time length of each of these

result_3 <- TVECM.HStest(hh_ttf[,c(1:2)])
result_4 <- TVECM.HStest(hh_jkm[,c(1:2)])

result_5 <- TVECM.HStest(hh_nbp[,c(1:2)])
result_5


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




library(a
# we can add in an intercept -- though we want the intercept to be 0: 


wald.test(Sigma = DOLS_result$varmat, b = DOLS_result$beta, Terms = 1:2)



# now the window

