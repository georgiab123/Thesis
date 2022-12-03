# lets get henry hub daily, weekly, monthly

# lets get crude daily, weekly, monthly

# TTF



# the mean -- daily, weekly, monthly
# no, we will use the
round(mean(ts_hh_outliers),2)

# check if the below has outliers removed
round(mean(ts_crude_final),2)


round(mean(nbp_ts_transform),2)
round(mean(NBP_ts),2) 
roudn(mean(ts_month_nbp),2)

round(mean(JK_ts),2)
round(mean(daily_JKM_ts),2)
round(mean(monthly_ts_jkm),2)

# check if the below has outliers removed. 
round(mean(eu_brent_ts),2)

round(mean(ttf_daily),2)
round(mean(ttf_dutch_ts),2)
round(mean(ts_month_ttf),2)

# the min
round(min(ts_hh_outliers),2)
round(min(ts_crude_final),2)
round(min(NBP_ts),2) 
round(min(JK_ts),2)
round(min(eu_brent_ts),2)
round(min(ttf_dutch_ts),2)

# the max
round(max(ts_hh_outliers),2)
round(max(ts_crude_final),2)
round(max(NBP_ts),2) 
round(max(JK_ts),2)
round(max(eu_brent_ts),2)
round(max(ttf_dutch_ts),2)

# the sd
round(sd(ts_hh_outliers),2)
round(sd(ts_crude_final),2)
round(sd(NBP_ts),2) 
round(sd(JK_ts),2)
round(sd(eu_brent_ts),2)
round(sd(ttf_dutch_ts),2)

# number of observations
length(ts_hh_outliers)
length(ts_crude_final) 
length(NBP_ts)
length(JK_ts)
length(eu_brent_ts)
length(ttf_dutch_ts)