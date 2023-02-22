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


# IMPORTING DATA ##########################################################

# all from local repostory -- generated using 04_break_testing script
# Henry Hub:
  # Daily

  # Weekly
  
  # Monthly

# TTF:
  # Daily

  # Weekly

  # Monthly

# JKM:
  # Daily

  # Weekly

  # Monthly

# NBP :
  # Daily

  # Weekly

  # Monthly

# WTI Crude:
  # Daily

  # Weekly

  # Monthly

# EU brent:
  # Daily

  # Weekly

  # Monthly

# World Bank -- only monthly


# run the summary statistics using my for loop:
for(i in ts_names){
  print(i)
  print(mean(get(i)))
  
}

# MEAN #####################################################################



round(mean(hh_prices_01),2)
round(mean(ts_hh_weekly),2)
round(mean(ts_hh_monthly),2)


round(mean(ts_wti_daily),2)
round(mean(ts_wti_weekly),2)
round(mean(ts_wti_monthly),2)

round(mean(nbp_ts_transform),2)
round(mean(NBP_ts),2) 
round(mean(ts_month_nbp),2)

round(mean(JK_ts),2)
round(mean(daily_JKM_ts),2)
round(mean(monthly_ts_jkm),2)

round(mean(brent_daily_ts), 2) 
round(mean(brent_week_ts),2)
round(mean(brent_month_ts),2)

round(mean(ttf_daily),2)
round(mean(ttf_dutch_ts),2)
round(mean(ts_month_ttf),2)

round(mean(wb_eu_ng),2)
round(mean(wb_japan_lng),2)


# MINIMUM #####################################################################
round(min(hh_prices_01),2)
round(min(ts_hh_weekly),2)
round(min(ts_hh_monthly),2)

# check if the below has outliers removed
round(min(ts_wti_daily),2)
round(min(ts_wti_weekly),2)
round(min(ts_wti_monthly),2)

round(min(nbp_ts_transform),2)
round(min(NBP_ts),2) 
round(min(ts_month_nbp),2)

round(min(JK_ts),2)
round(min(daily_JKM_ts),2)
round(min(monthly_ts_jkm),2)

# check if the below has outliers removed. 
round(min(brent_daily_ts), 2) 
round(min(brent_week_ts),2)
round(min(brent_month_ts),2)

round(min(ttf_daily),2)
round(min(ttf_dutch_ts),2)
round(min(ts_month_ttf),2)

round(min(wb_eu_ng),2)
round(min(wb_japan_lng),2)

# MAXIMUM #####################################################################
round(max(hh_prices_01),2)
round(max(ts_hh_weekly),2)
round(max(ts_hh_monthly),2)


round(max(ts_wti_daily),2)
round(max(ts_wti_weekly),2)
round(max(ts_wti_monthly),2)

round(max(nbp_ts_transform),2)
round(max(NBP_ts),2) 
round(max(ts_month_nbp),2)

round(max(JK_ts),2)
round(max(daily_JKM_ts),2)
round(max(monthly_ts_jkm),2)

round(max(brent_daily_ts), 2) 
round(max(brent_week_ts),2)
round(max(brent_month_ts),2)

round(max(ttf_daily),2)
round(max(ttf_dutch_ts),2)
round(max(ts_month_ttf),2)

round(max(wb_eu_ng),2)
round(max(wb_japan_lng),2)

# SD ##########################################################################

round(sd(hh_prices_01),2)
round(sd(ts_hh_weekly),2)
round(sd(ts_hh_monthly),2)


round(sd(ts_wti_daily),2)
round(sd(ts_wti_weekly),2)
round(sd(ts_wti_monthly),2)

round(sd(nbp_ts_transform),2)
round(sd(NBP_ts),2) 
round(sd(ts_month_nbp),2)

round(sd(JK_ts),2)
round(sd(daily_JKM_ts),2)
round(sd(monthly_ts_jkm),2)

round(sd(brent_daily_ts), 2) 
round(sd(brent_week_ts),2)
round(sd(brent_month_ts),2)

round(sd(ttf_daily),2)
round(sd(ttf_dutch_ts),2)
round(sd(ts_month_ttf),2)

round(sd(wb_eu_ng),2)
round(sd(wb_japan_lng),2)

# no of observations ##########################################################

length(hh_prices_01) 
length(ts_hh_weekly)
length(ts_hh_monthly)


length(ts_wti_daily)
length(ts_wti_weekly)
length(ts_wti_monthly)

length(nbp_ts_transform)
length(NBP_ts)
length(ts_month_nbp)

length(JK_ts)
length(daily_JKM_ts)
length(monthly_ts_jkm)

length(brent_daily_ts) 
length(brent_week_ts)
length(brent_month_ts)

length(ttf_daily)
length(ttf_dutch_ts)
length(ts_month_ttf)

length(wb_eu_ng)
length(wb_japan_lng)
       

# START/END DATES ############################################

head(hh_prices_01) 
head(ts_hh_weekly)
tail(ts_hh_weekly)

head(ts_hh_monthly)
tail(ts_hh_monthly)

head(ts_wti_daily)
tail(ts_wti_daily)

head(ts_wti_weekly)
tail(ts_wti_weekly)

head(ts_wti_monthly)
tail(ts_wti_monthly)

head(nbp_ts_transform)
tail(nbp_ts_transform)

head(NBP_ts)
tail(NBP_ts)

head(ts_month_nbp)
tail(ts_month_nbp)

head(daily_JKM_ts)
tail(daily_JKM_ts)

head(JK_ts)
tail(JK_ts)

head(monthly_ts_jkm)
tail(monthly_ts_jkm)

head(brent_daily_ts) 
tail(brent_daily_ts) 


head(brent_week_ts)
tail(brent_week_ts)

head(brent_month_ts)
tail(brent_month_ts)

head(ttf_daily)
tail(ttf_daily)

head(ttf_dutch_ts)
tail(ttf_dutch_ts)


head(ts_month_ttf)
tail(ts_month_ttf)

head(wb_eu_ng)
tail(wb_eu_ng)
length(wb_japan_lng)
       
       
      
       