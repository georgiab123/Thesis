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

# all data loaded in from 01_env

# HOMOSKEDASTICITY CHECK  ######################################################

diffed_HH <- diff(ts_hh_weekly)

t <- time(ts_hh_weekly)
reg0 <- lm(ts_hh_weekly~ t ) 
plot(reg0$fitted, reg0$residuals,main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitte
acf(reg0$residuals,main="ACF plot of Residuals")#sample acf plot 
shapiro.test(reg0$residuals)

model <- lm(ts_hh_weekly~t)
bptest(naive(ts_hh_monthly))

month <- as.factor(cycle(diffed_HH ))
reg1 <- lm(diffed_HH ~t + month) #
plot(reg1$fitted, reg1$residuals,main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitte
acf(reg1$residuals,main="ACF plot of Residuals")#sample acf plot 
shapiro.test(reg1$residuals)

decomposedRes <- decompose(hh_prices_01, type="mult") 
stlRes <- stl(hh_prices_01, s.window = "periodic")
plot(decomposedRes)

res <- residuals(naive(ts_hh_weekly))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")

res <- residuals(naive(ts_hh_monthly))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")
gghistogram(res) + ggtitle("Histogram of residuals")
ggAcf(res) + ggtitle("ACF of residuals")


# ADF & PP TESTING  ################################################################
# default PP and defauly ADF
# hh daily
adf.test(hh_prices_01)
round(0.01085,3)
pp.test(hh_prices_01, type = "Z(alpha)")
pp.test(hh_prices_01, type = "Z(t_alpha)")



pp.test(hh_prices_01, type = "Z(alpha)")
aTSA::pp.test(hh_prices_01)
# first diff:
adf.test(diff(hh_prices_01))
pp.test(diff(hh_prices_01))]

kpss.test(hh_prices_01)
kpss.test(diff(hh_prices_01))

# hh wekly
adf.test(ts_hh_weekly)
pp.test(ts_hh_weekly)
adf.test(diff(ts_hh_weekly))
pp.test(diff(ts_hh_weekly))
kpss.test(ts_hh_weekly)
kpss.test(diff(ts_hh_weekly))

adf.test(ts_hh_monthly)
pp.test(ts_hh_monthly)
aTSA::pp.test(ts_hh_monthly)
adf.test(diff(ts_hh_monthly))
pp.test(diff(ts_hh_monthly))
kpss.test(ts_hh_monthly)
kpss.test(diff(ts_hh_monthly))

# WTI CRUDE

adf.test(ts_wti_daily)
pp.test(ts_wti_daily)
adf.test(diff(ts_wti_daily))
pp.test(diff(ts_wti_daily))
kpss.test(ts_wti_daily)
kpss.test(diff(ts_wti_daily))

adf.test(ts_wti_weekly)
pp.test(ts_wti_weekly)
adf.test(diff(ts_wti_weekly))
pp.test(diff(ts_wti_weekly))
kpss.test(ts_wti_weekly)
kpss.test(diff(ts_wti_weekly))

adf.test(ts_wti_monthly)
pp.test(ts_wti_monthly)
adf.test(diff(ts_wti_monthly))
pp.test(diff(ts_wti_monthly))
kpss.test(ts_wti_monthly)
kpss.test(diff(ts_wti_monthly))


# NBP

adf.test(nbp_ts_transform)
pp.test(nbp_ts_transform)
adf.test(diff(nbp_ts_transform))
pp.test(diff(nbp_ts_transform))
kpss.test(nbp_ts_transform)
kpss.test(diff(nbp_ts_transform))

adf.test(NBP_ts)
pp.test(NBP_ts)
adf.test(diff(NBP_ts))
pp.test(diff(NBP_ts))
kpss.test(NBP_ts)
kpss.test(diff(NBP_ts))

adf.test(ts_month_nbp)
pp.test(ts_month_nbp)
adf.test(diff(ts_month_nbp))
pp.test(diff(ts_month_nbp))
kpss.test(ts_month_nbp)
kpss.test(diff(ts_month_nbp))


#JKM
adf.test(daily_JKM_ts)
pp.test(daily_JKM_ts)
adf.test(diff(daily_JKM_ts))
pp.test(diff(daily_JKM_ts))
kpss.test(daily_JKM_ts)
kpss.test(diff(daily_JKM_ts))

adf.test(JK_ts)
pp.test(JK_ts)
adf.test(diff(JK_ts))
pp.test(diff(JK_ts))
kpss.test(JK_ts)
kpss.test(diff(JK_ts))

adf.test(monthly_ts_jkm)
pp.test(monthly_ts_jkm)
adf.test(diff(monthly_ts_jkm))
pp.test(diff(monthly_ts_jkm))
kpss.test(monthly_ts_jkm)
kpss.test(diff(monthly_ts_jkm))

# EU BRENT

adf.test(brent_daily_ts)
pp.test(brent_daily_ts)
adf.test(diff(brent_daily_ts))
pp.test(diff(brent_daily_ts))
kpss.test(brent_daily_ts)
kpss.test(diff(brent_daily_ts))

adf.test(brent_week_ts)
pp.test(brent_week_ts)
adf.test(diff(brent_week_ts))
pp.test(diff(brent_week_ts))
kpss.test(brent_week_ts)
kpss.test(diff(brent_week_ts))

adf.test(brent_month_ts)
pp.test(brent_month_ts)
adf.test(diff(brent_month_ts))
pp.test(diff(brent_month_ts))
kpss.test(brent_month_ts)
kpss.test(diff(brent_month_ts))


# TTF
adf.test(ttf_daily)
pp.test(ttf_daily)
adf.test(diff(ttf_daily))
pp.test(diff(ttf_daily))
kpss.test(ttf_daily)
kpss.test(diff(ttf_daily))

adf.test(ttf_dutch_ts)
pp.test(ttf_dutch_ts)
adf.test(diff(ttf_dutch_ts))
pp.test(diff(ttf_dutch_ts))
kpss.test(ttf_dutch_ts)
kpss.test(diff(ttf_dutch_ts))

adf.test(ts_month_ttf)
pp.test(ts_month_ttf)
adf.test(diff(ts_month_ttf))
pp.test(diff(ts_month_ttf))
kpss.test(ts_month_ttf)
kpss.test(diff(ts_month_ttf))


# EU Natural Gas

adf.test(wb_eu_ng)
pp.test(wb_eu_ng)
adf.test(diff(wb_eu_ng))
pp.test(diff(wb_eu_ng))
kpss.test(wb_eu_ng)
kpss.test(diff(wb_eu_ng))

# Japan LNG

adf.test(wb_japan_lng)
pp.test(wb_japan_lng)
adf.test(diff(wb_japan_lng))
pp.test(diff(wb_japan_lng))
kpss.test(wb_japan_lng)
kpss.test(diff(wb_japan_lng))



