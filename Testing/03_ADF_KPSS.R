
# LOADING PACKAGES ############################################################
library(dplyr)
library(lubridate)
library(anytime)
library(ggfortify)
library(stats)
library(strucchange)
library(forecast)
library(tseries)
library(data.table)
library(lmtest)
library(aTSA)


# ADF TESTS #####################################################################

# WEEKLY #######################################################################

adf.test(hh_weekly_real)
adf.test(ttf_weekly_real)
adf.test(nbp_weekly_real)
adf.test(jkm_weekly_real)
adf.test(wti_weekly_real)
adf.test(brent_weekly_real)

adf.test(diff(hh_weekly_real))
adf.test(diff(ttf_weekly_real))
adf.test(diff(nbp_weekly_real))
adf.test(diff(jkm_weekly_real))
adf.test(diff(wti_weekly_real))
adf.test(diff(brent_weekly_real))


# DAILY #######################################################################

adf.test(hh_daily_real)
adf.test(ttf_daily_real)
adf.test(nbp_daily_real)
adf.test(jkm_daily_real)
adf.test(wti_daily_real)
adf.test(brent_daily_real)

adf.test(diff(hh_daily_real))
adf.test(diff(ttf_daily_real))
adf.test(diff(nbp_daily_real))
adf.test(diff(jkm_daily_real))
adf.test(diff(wti_daily_real))
adf.test(diff(brent_daily_real))


# MONTHLY #######################################################################

adf.test(hh_monthly_real)
adf.test(ttf_monthly_real)
adf.test(nbp_monthly_real)
adf.test(jkm_monthly_real)
adf.test(wti_monthly_real)
adf.test(brent_monthly_real)

adf.test(diff(hh_monthly_real))
adf.test(diff(ttf_monthly_real))
adf.test(diff(nbp_monthly_real))
adf.test(diff(jkm_monthly_real))
adf.test(diff(wti_monthly_real))
adf.test(diff(brent_monthly_real))





