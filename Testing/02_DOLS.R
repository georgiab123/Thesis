# LOADING PACKAGES

library(cointReg)

# LOADING IN DATA

# CONSTRUCTING DOLS REGRESSIONS 
 
# functionality is as below 
cointRegD(x, y, deter, kernel = c("ba", "pa", "qs", "tr"),
          bandwidth = c("and", "nw"), n.lead = NULL, n.lag = NULL,
          kmax = c("k4", "k12"), info.crit = c("AIC", "BIC"), demeaning = FALSE,
          check = TRUE, ...)

# set up our y and x variables

# some times:
  # time(hh_monthly)[276] <- 2020 
start <- window()
end <- window( )

length(hh_monthly_log)
y <- window(hh_monthly_log, start = 2018, end = time(hh_monthly_log)[309])   
x <- window(nbp_monthly_log, start = 2018) 
n <- length(y)

# set up deter matrix

deter <-  cbind(level = rep(1,n), trend = 1:n)

# now running regression
DOLS_result <- cointRegD(x, y, deter, kmax = c("k4"), bandwidth = c("nw"))
