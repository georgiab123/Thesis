# LOADING PACKAGES

library(cointReg)
library(aTSA)
library(lubridate)
library(fBasics)
library(splusTimeSeries)
library(aod)
library

# LOADING IN DATA

# CONSTRUCTING DOLS REGRESSIONS 
 
# functionality is as below 
#cointRegD(x, y, deter, kernel = c("ba", "pa", "qs", "tr"),
#          bandwidth = c("and", "nw"), n.lead = NULL, n.lag = NULL,
#          kmax = c("k4", "k12"), info.crit = c("AIC", "BIC"), demeaning = FALSE,
#          check = TRUE, ...)

# set up our y and x variables

# some times:
# time(hh_monthly)[276] <- 2020 

# DAILY DOLS BASED ON MAKI

# splits chosen based on model 3 (or model 2 for ttf)
head(date_decimal(as.numeric(time(hh_daily_log))))


# JKM 
# 06-Feb-2022 -- onwards
# START 2022-02-06 23:59:59 UTC" -- 9132  
# END 2022-11-01 23:59:59 UTC 
y <- window(hh_daily_log, start = time(hh_daily_log)[9132],  end = time(jkm_daily_log)[3014])   
x <- window(jkm_daily_log, start = time(hh_daily_log)[9132], end = time(jkm_daily_log)[3014]) 
length(x) == length(y)
n <- length(y)
deter <- cbind(level = rep(1,n), trend = 1:n)
jkm_5 <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"))
jkm_5

# START 2021-05-01 23:59:59 UTC" -- 9132  
# END  2022-02-06

y <- window(hh_daily_log, start = time(jkm_daily_log)[2465] ,  end = time(hh_daily_log)[9132])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[2465], end = time(hh_daily_log)[9132]) 
length(x) == length(y)
n <- length(y)
deter <- cbind(level = rep(1,n), trend = 1:n)
jkm_4 <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"))
jkm_4


# end 2021-05-01 23:59:59 UTC" -- 9132  
# start  2020-10-13

y <- window(hh_daily_log, start = time(jkm_daily_log)[2265] ,  end = time(jkm_daily_log)[2465])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[2265], end = time(jkm_daily_log)[2465]) 
length(x) == length(y)
n <- length(y)
deter <- cbind(level = rep(1,n), trend = 1:n)
jkm_3 <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"))
jkm_3

# end 2020-10-13 23:59:59 UTC" -- 9132  
# start  2020-03--02

y <- window(hh_daily_log, start = time(jkm_daily_log)[2040] ,  end = time(jkm_daily_log)[2265])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[2040], end = time(jkm_daily_log)[2265]) 
length(x) == length(y)
n <- length(y)
deter <- cbind(level = rep(1,n), trend = 1:n)
jkm_2 <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"))
jkm_2

# end 2020-03--02
# start  2016-06-02

y <- window(hh_daily_log, start = time(jkm_daily_log)[672] ,  end = time(jkm_daily_log)[2040])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[672], end = time(jkm_daily_log)[2040]) 
length(x) == length(y)
n <- length(y)
deter <- cbind(level = rep(1,n), trend = 1:n)
jkm_1 <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"))
jkm_1

# end 2016-06-02
# start = 2016-06-02

y <- window(hh_daily_log, start = time(jkm_daily_log)[1] ,  end = time(jkm_daily_log)[672])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[1], end = time(jkm_daily_log)[672]) 
length(x) == length(y)
n <- length(y)
deter <- cbind(level = rep(1,n), trend = 1:n)
jkm_0 <- cointRegD(x, y, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"))
jkm_0





# GREGORY HANSEN DOLS ########################################################################33####

# for all the below we do a DOLS with an intercept and then a DOLS with an intercept
# and a time trend 


# BRENT 

# brent 0th period 
# split = 21-March-2009 


y <- window(hh_daily_log, start = time(hh_daily_log)[1] ,  end = time(hh_daily_log)[4430])   
x <- window(brent_daily_log, start = time(hh_daily_log)[1], end = time(hh_daily_log)[4430]) 
n <- length(y)
# with time trend
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_0_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4") ,  info.crit = "AIC" )
brent_0_gh_trend 

deter <- cbind(level = rep(1,n), trend = 1:n )
brent_0_gh_trend_bic <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4") ,  info.crit = "BIC" )
brent_0_gh_trend_bic

# no time trend
deter <- cbind(level = rep(1,n))
brent_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC" )
brent_0_gh

deter <- cbind(level = rep(1,n))
brent_0_gh_BIC <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC" )
brent_0_gh_BIC



plot(brent_0_gh_trend_bic, ylim=c(-2,2), col = "blue") # with trend
par(new = TRUE)
plot(brent_0_gh_trend, ylim=c(-2,2), col = "red") # without trend
par(new = TRUE)
plot(brent_0_gh, ylim=c(-2,2), col = "purple") # without trend
par(new = TRUE)
plot(brent_0_gh_BIC, ylim=c(-2,2), col = "green") # without trend

# brent after break 
y <- window(hh_daily_log, start = time(hh_daily_log)[4430] ,  end = time(hh_daily_log)[9425])   
x <- window(brent_daily_log, start = time(hh_daily_log)[4430], end = time(hh_daily_log)[9425]) 
n <- length(y)
# with time trend
deter <- cbind(level = rep(1,n), trend = 1:n)
brent_1_gh_trend <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
brent_1_gh_trend

#no time trend 
deter <- cbind(level = rep(1,n))
brent_1_gh <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
brent_1_gh

# plot the residuals
plot(brent_1_gh_trend, ylim=c(-0.9,0.9), col = "blue") # with trend
par(new = TRUE)
plot(brent_1_gh, ylim=c(-0.9,0.9), col = "red") # without trend

# WTI 
#-- split = 21 March 2009  
y <- window(hh_daily_log, start = time(hh_daily_log)[1] ,  end = time(hh_daily_log)[4430])   
x <- window(wti_daily_log, start = time(hh_daily_log)[1], end = time(hh_daily_log)[4430]) 
n <- length(y)
# with time trend
deter <- cbind(level = rep(1,n), trend = 1:n)
wti_0_gh_trend <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
wti_0_gh_trend
# no time trend 
deter <- cbind(level = rep(1,n))
wti_0_gh <- cointRegD(x, y, deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
wti_0_gh

# wti 1 # with trend has better results
y <- window(hh_daily_log, start = time(hh_daily_log)[4430] ,  end = time(hh_daily_log)[9425])   
x <- window(wti_daily_log, start = time(hh_daily_log)[4430], end = time(hh_daily_log)[9425]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n)
wti_1_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC"  )
wti_1_gh_trend

deter <- cbind(level = rep(1,n))
wti_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4") , info.crit = "BIC"  )
wti_1_gh


# JKM

# before break 
# start  = start of jkm 2014/07/31
# end  =  2016/05/27 
y <- window(hh_daily_log, start = time(jkm_daily_log)[1] ,  end = time(hh_daily_log)[7053])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[1], end = time(hh_daily_log)[7053]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_0_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_0_gh_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_0_gh

# start = 2016-05-27
# end = end of JKM sample period 
y <- window(hh_daily_log, start = time(hh_daily_log)[7053] ,  end = time(jkm_daily_log)[3014])   
x <- window(jkm_daily_log, start = time(hh_daily_log)[7053], end = time(jkm_daily_log)[3014]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_1_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_1_gh_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
jkm_1_gh


# TTF 

# start  = start of ttf sample period ~ 2010 
# end  =  13th July 2012 or 2012-07-13
y <- window(hh_daily_log, start = time(ttf_daily_log)[1] ,  end = time(ttf_daily_log)[923])   
x <- window(ttf_daily_log, start = time(ttf_daily_log)[1], end = time(ttf_daily_log)[923]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_0_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"), info.crit = "BIC")
ttf_0_gh_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"), info.crit = "BIC")
ttf_0_gh

# start = 2012-07-13
# end = end of JKM sample period 
y <- window(hh_daily_log, start =  time(ttf_daily_log)[923] ,  end = time(ttf_daily_log)[length(ttf_daily_log)])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[923], end = time(ttf_daily_log)[length(ttf_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_1_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_1_gh_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
ttf_1_gh

# NBP
# choose breakdate of 01-Sep-2010  or 2010-09-01

# start  = start of nbp sample period ~ 2010 
# end  =  13th July 2012 or 2012-07-13
y <- window(hh_daily_log, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[4959])   
x <- window(nbp_daily_log, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[4959]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_0_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_gh_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_0_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_0_gh

# start = 2012-07-13
# end = end of JKM sample period 
y <- window(hh_daily_log, start =  time(nbp_daily_log)[4959] ,  end = time(nbp_daily_log)[length(nbp_daily_log)])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[4959], end = time(nbp_daily_log)[length(nbp_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_1_gh_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "BIC")
nbp_1_gh_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_1_gh <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k12"),  info.crit = "BIC")
nbp_1_gh


# MAKI TEST###############################################################################


date_decimal(as.numeric(time(nbp_daily_log)))[3524]

# NBP
# choose breakdates of 
# 2006-09-26, 2008-01-27, 2016-06-03, 2018-01-01, 2021-02-15  MODEL 3 

# end  = 2006-09-26
y <- window(hh_daily_log, start = time(nbp_daily_log)[1] ,  end = time(nbp_daily_log)[3524])   
x <- window(nbp_daily_log, start = time(nbp_daily_log)[1], end = time(nbp_daily_log)[3524]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_0_m

# end = 2008-01-27
y <- window(hh_daily_log, start =  time(nbp_daily_log)[3524] ,  end = time(nbp_daily_log)[4011])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[3524], end = time(nbp_daily_log)[4011]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_1_m

# end = 2016-06-03
y <- window(hh_daily_log, start =  time(nbp_daily_log)[4011] ,  end = time(nbp_daily_log)[7059])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[4011], end = time(nbp_daily_log)[7059]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_2_m

# end = 2018-01-01,
y <- window(hh_daily_log, start =  time(nbp_daily_log)[7059] ,  end = time(nbp_daily_log)[7635])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[7059], end = time(nbp_daily_log)[7635]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_3_m

# end = 2021-02-15
y <- window(hh_daily_log, start =  time(nbp_daily_log)[7635] ,  end = time(nbp_daily_log)[8775])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[7635], end = time(nbp_daily_log)[8775]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(nbp_daily_log)[8775] ,  end = time(nbp_daily_log)[length(nbp_daily_log)])   
x <- window(nbp_daily_log, start =  time(nbp_daily_log)[8775], end = time(nbp_daily_log)[length(nbp_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
nbp_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
nbp_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
nbp_5_m


# TTF

date_decimal(as.numeric(time(nbp_daily_log)))[3524]

#TTF 

# end  = 2011-11-24 
y <- window(hh_daily_log, start = time(ttf_daily_log)[1] ,  end = time(ttf_daily_log)[692])   
x <- window(ttf_daily_log, start = time(ttf_daily_log)[1], end = time(ttf_daily_log)[692]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_0_m

# end = 2014 - 07 -03 
y <- window(hh_daily_log, start =  time(ttf_daily_log)[692] ,  end = time(ttf_daily_log)[1643])   
x <- window(nbp_daily_log, start =  time(ttf_daily_log)[692], end = time(ttf_daily_log)[1643]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_1_m

# end = 2015-03-01
y <- window(hh_daily_log, start =  time(ttf_daily_log)[1643] ,  end = time(ttf_daily_log)[1884])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[1643], end = time(ttf_daily_log)[1884]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_2_m

# end = 2015-12-13
y <- window(hh_daily_log, start =  time(ttf_daily_log)[1884] ,  end = time(ttf_daily_log)[2171])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[1884], end = time(ttf_daily_log)[2171]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_3_m

# end = 2022-02-01
y <- window(hh_daily_log, start =  time(ttf_daily_log)[2171] ,  end = time(ttf_daily_log)[4411])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[2171], end = time(ttf_daily_log)[4411]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(ttf_daily_log)[4411] ,  end = time(ttf_daily_log)[length(ttf_daily_log)])   
x <- window(ttf_daily_log, start =  time(ttf_daily_log)[4411], end = time(ttf_daily_log)[length(ttf_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
ttf_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
ttf_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
ttf_5_m


#JKM 


date_decimal(as.numeric(time(jkm_daily_log)))[3524]

# end  = 2016-06-02
y <- window(hh_daily_log, start = time(jkm_daily_log)[1] ,  end = time(jkm_daily_log)[672])   
x <- window(jkm_daily_log, start = time(jkm_daily_log)[1], end = time(jkm_daily_log)[672]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_0_m

# end = 2020-03-02
y <- window(hh_daily_log, start =  time(jkm_daily_log)[672] ,  end = time(jkm_daily_log)[2040])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[672], end = time(jkm_daily_log)[2040]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_1_m

# end = 2020-10-13
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2040] ,  end = time(jkm_daily_log)[2265])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2040], end = time(jkm_daily_log)[2265]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_2_m

# end = 2021-05-01
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2265] ,  end = time(jkm_daily_log)[2465])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2265], end = time(jkm_daily_log)[2465]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_3_m

# end = 2022-02-06
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2465] ,  end = time(jkm_daily_log)[2746])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2465], end = time(jkm_daily_log)[2746]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(jkm_daily_log)[2746] ,  end = time(jkm_daily_log)[length(jkm_daily_log)])   
x <- window(jkm_daily_log, start =  time(jkm_daily_log)[2746], end = time(jkm_daily_log)[length(jkm_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
jkm_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
jkm_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
jkm_5_m



# BRENT 

date_decimal(as.numeric(time(brent_daily_log)))[2818]

# end  = 2003-02-27
y <- window(hh_daily_log, start = time(brent_daily_log)[1] ,  end = time(brent_daily_log)[2218])   
x <- window(brent_daily_log, start = time(brent_daily_log)[1], end = time(brent_daily_log)[2218]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_0_m

# end = 2004-10-20
y <- window(hh_daily_log, start =  time(brent_daily_log)[2218] ,  end = time(brent_daily_log)[2818])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[2218], end = time(brent_daily_log)[2818]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_1_m

# end = 2007-02-06
y <- window(hh_daily_log, start =  time(brent_daily_log)[2818] ,  end = time(brent_daily_log)[3657])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[2818], end = time(brent_daily_log)[3657]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_2_m

# end = 2018-11-18
y <- window(hh_daily_log, start =  time(brent_daily_log)[3657] ,  end = time(brent_daily_log)[7957])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[3657], end = time(brent_daily_log)[7957]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_3_m

# end = 2021-02-15
y <- window(hh_daily_log, start =  time(brent_daily_log)[7957] ,  end = time(brent_daily_log)[8776])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[7957], end = time(brent_daily_log)[8776]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_4_m

# end = end of sample data
y <- window(hh_daily_log, start =  time(brent_daily_log)[8776],  end = time(hh_daily_log)[length(hh_daily_log)])   
x <- window(brent_daily_log, start =  time(brent_daily_log)[8776], end = time(hh_daily_log)[length(hh_daily_log)]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
brent_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
brent_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
brent_5_m


# WTI

date_decimal(as.numeric(time(brent_daily_log)))[2818]

# end  = 2003-01-22
y <- window(hh_daily_log, start = time(wti_daily_log)[1] ,  end = time(wti_daily_log)[2182])   
x <- window(wti_daily_log, start = time(wti_daily_log)[1], end = time(wti_daily_log)[2182]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_0_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_0_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_0_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_0_m

# end = 2004-10-20 (SAME)
y <- window(hh_daily_log, start =  time(wti_daily_log)[2182] ,  end = time(brent_daily_log)[2818])   
x <- window(wti_daily_log, start =  time(wti_daily_log)[2182], end = time(brent_daily_log)[2818]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_1_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_1_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_1_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_1_m

# end = 2007-03-16
y <- window(hh_daily_log, start =  time(brent_daily_log)[2818] ,  end = time(wti_daily_log)[3695])   
x <- window(wti_daily_log, start =  time(brent_daily_log)[2818], end = time(wti_daily_log)[3695]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_2_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_2_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_2_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_2_m

# end = 2018-01-15
y <- window(hh_daily_log, start =  time(wti_daily_log)[3695] ,  end = time(wti_daily_log)[7650])   
x <- window(wti_daily_log, start =  time(wti_daily_log)[3695], end = time(wti_daily_log)[7650]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_3_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_3_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_3_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_3_m

# end = 2021-02-15 (SAME)
y <- window(hh_daily_log, start =  time(wti_daily_log)[7650] ,  end = time(brent_daily_log)[8776])   
x <- window(wti_daily_log, start =  time(wti_daily_log)[7650], end = time(brent_daily_log)[8776]) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_4_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_4_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
wti_4_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_4_m

# end = end of sample data
y <- matrix(window(hh_daily_log, start =  time(brent_daily_log)[8776],  end = time(hh_daily_log)[length(hh_daily_log)]), ncol = 1)   
x <- matrix(window(wti_daily_log, start =  time(brent_daily_log)[8776], end = time(hh_daily_log)[length(hh_daily_log)]), ncol = 1) 
n <- length(y)
# with time trend 
deter <- cbind(level = rep(1,n), trend = 1:n )
wti_5_m_trend <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC")
wti_5_m_trend
# with no time trend 
deter <- cbind(level = rep(1,n))
cointReg:::getLeadLag(x= x, y =y, deter=deter,  max.lag = 10, max.lead = 10, ic = c("BIC"), symmet = TRUE)
wti_5_m <- cointRegD(x, y,  deter, bandwidth = c("nw"), kernel = "ba", kmax = c("k4"),  info.crit = "AIC", n.lead = 1, n.lag =1)
wti_5_m
plot(wti_5_m_trend, ylim=c(-0.7,0.7), col = "blue") # with trend
par(new = TRUE)
plot(wti_5_m, ylim=c(-0.7,0.7), col = "red") # without trend

# WALD TESTS ###############################################################################

wtest <- wald.test(Sigma = wti_1_hj$varmat, b = wti_1_hj$theta.all, Terms = 3, H0 = c(1))

# comparision to manual

lags_x <- tslag(diff(x), -3:3 )
colnames(lags_x)
colnames(lags_x) <- c("lead_3", "lead_2", "lead1", "lag_0", "lag_1", "lag_2", "lag_3")
lags_x <- data.frame(lags_x)


# remove the first observations so aligns with differenced data
dols.fit = lm(y[-1] ~ x[-1] + lags_x$lead_3 +  lags_x$lead_2 + 
                 lags_x$lead1 + lags_x$lag_1 + lags_x$lag_2 + 
                 lags_x$lag_3, na.rm = T) 
summary(dols.fit)
summary(dols.fit,correction="nw", kernel = "ba")

deter <- cbind(level = rep(1,n))

jkm_5 <- cointRegD(x, y, deter, bandwidth = c("nw"),  kernel = "ba", kmax = "k12")

# wald test:
# if less than 0.05, reject the null hypothesis that the coefficient is 
# as specified in H0

wtest <- wald.test(Sigma = jkm_5$varmat, b = jkm_5$theta.all, Terms = 1:2, H0 = c(0,1))
print(wtest, digits = 20)

# 01 - May -2021

# 13 - Oct -2020

# 02 - Marh -2020

# 02-June-2016 

# set up deter matrix

deter <-  cbind(level = rep(1,n), trend = 1:n)

# now running regression
DOLS_result <- cointRegD(x, y, deter, kmax = c("k4"), bandwidth = c("nw"))

result <-coint.test(hh_daily_log, wti_daily_log, d = 0, nlag = 10, output = TRUE)


s# testing regular cointegration: trend <- (1: length(hh_monthly))
model <- lm(hh_daily_log ~ 1 + wti_daily_log + trend)
model
plot(residuals(model), type = "l")
adf(residuals(model))
