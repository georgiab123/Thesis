
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
library(tseries)

# TESTING #####################################################################


po.test(cbind(ts_hh_monthly, ts_wti_monthly))
summary(urca::ca.po(cbind(ts_hh_monthly, ts_wti_monthly)))

