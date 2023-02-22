# IMPORTING LIBRARIES

library(fpp2)

# DATA 

ts_names <- c()

# HENRY HUB #############
# daily henry hub
term_ts_hh
# remove the strong outliers
index_max <- which.max(term_ts_hh)
index_max
# hh_prices_00 is the underlying time series
# there is a spike on february the 16th at index_max of 8782? 
# thus we remove 8781, 8782, and 8783
test <- term_ts_hh
test[8782] <- NA
test[8781] <- NA
test[8783] <- NA
index_max <- which.max(text)
# there is another spike in 2003
text[2215] <- NA
text_01 <- na.interp(text, linear = TRUE)
hh_daily <- text_01

# hh_daily 
hh_daily_log <- log(hh_daily)
plot(hh_daily_log)
ts_names <- append(ts_names, "hh_daily")
ts_names <- append(ts_names, "hh_daily_log")

# hh _ weekly
hh_prices_00$price <- hh_daily
week <- as.Date(cut(hh_prices_00$time, "week"))
weekly_mean_hh <- aggregate(price ~ week,  hh_prices_00, mean)

#now we convert to a time series
hh_weekly <- ts(weekly_mean_hh$price, start=c(1997, 05), end = c(2022, 47), frequency=52)
hh_weekly_log <- log(hh_weekly)
ts_names <- append(ts_names, "hh_weekly")
ts_names <- append(ts_names, "hh_weekly_log")

# now we convert to momthly
month <- as.Date(cut(hh_prices_00$time, "month"))
monthly_mean_hh <- aggregate(price ~ month,  hh_prices_00, mean)
hh_monthly <- ts(monthly_mean_hh$price, start=c(1997, 02), frequency=12)
hh_monthly_log <- log(hh_monthly)
ts_names <- append(ts_names, "hh_monthly")
ts_names <- append(ts_names, "hh_monthly_log")


# the following code writes each of these to a VSC
write.csv(data.frame(Y=as.matrix(hh_monthly), date=as.Date(as.yearmon(time(hh_monthly)))), file="forecast.csv")


# CRUDE WTI ####################################################



# NBP ####################################################

# daily
nbp_ts_og <- ts(nbp_daily_ts$price, start = c(1997, 32 ), frequency = 365)
nbp_daily <- na.interp(nbp_ts_og, linear = TRUE)
nbp_daily_log <- log(nbp_daily)
ts_names <- append(ts_names, "nbp_daily")
ts_names <- append(ts_names, "nbp_daily_log")

# weekly
nbp_daily_ts$price <-  nbp_daily
week <- as.Date(cut(nbp_daily_ts$time, "week"))
nbp_weekly<- aggregate(price ~ week,  nbp_daily_ts, mean)

nbp_weekly <- nbp_weekly [-1,] # start exactly on 1997-02-03 
nbp_weekly <- ts(nbp_weekly$price, start = c(1997,02), frequency = 52)
nbp_weekly_log <- log(nbp_weekly)
ts_names <- append(ts_names, "nbp_weekly")
ts_names <- append(ts_names, "nbp_weekly_log")

# monthly

month <- as.Date(cut(nbp_daily_ts$time, "month"))
nbp_monthly <- aggregate(price ~ month,  nbp_daily_ts, mean)
nbp_monthly <- ts(nbp_monthly$price, start = c(1997, 02), frequency = 12)
nbp_monthly_log <- log(nbp_monthly)
ts_names <- append(ts_names, "nbp_monthly")
ts_names <- append(ts_names, "nbp_monthly_log")

# TTF ####################################################

#daily 
ttf_daily_copy <- ttf_ts_df
# there is a series of outliers around: 
#2976 2018-02-25  9.168059
#2977 2018-02-26 12.330930
#2978 2018-02-27 16.116157
#2979 2018-02-28 27.322622
#2980 2018-03-01 10.287755
#2981 2018-03-02        NA
#2982 2018-03-03        NA
#2983 2018-03-04  7.338526
# we remove 2979
ttf_daily_copy$price[2979] <- NA # remove this value
ttf_daily_copy$price[2978] <- NA # remove this value
ttf_daily_copy[2976:2983,] # now check
ttf_daily <- ts(ttf_daily_copy[,2], start = c(2010, 03), frequency = 365) #convert to ts
ttf_daily <- na.interp(ttf_daily, linear = TRUE)
ttf_daily_log <- log(ttf_daily)
ts_names <- append(ts_names, "ttf_daily")
ts_names <- append(ts_names, "ttf_daily_log")

# weekly
ttf_daily_copy$price <-  ttf_daily
week <- as.Date(cut(ttf_daily_copy$time, "week"))
ttf_weekly<- aggregate(price ~ week,  ttf_daily_copy, mean)
ttf_weekly <- ts(ttf_weekly$price, start = c(2010, 1), end = c(2022, 46), frequency = 52)
ttf_weekly_log <- log(ttf_weekly)
ts_names <- append(ts_names, "ttf_weekly")
ts_names <- append(ts_names, "ttf_weekly_log")

# monthly

month <- as.Date(cut(ttf_daily_copy$time, "month"))
ttf_monthly <- aggregate(price ~ month,  ttf_daily_copy, mean)
ttf_monthly <- ts(ttf_monthly$price, start = c(2010,1), frequency = 12) 
ttf_monthly_log <- log(ttf_monthly)
ts_names <- append(ts_names, "ttf_monthly")
ts_names <- append(ts_names, "ttf_monthly_log")

# JKM  ########################################################

JKM_df <- japan_korea_df[-c(1:675),]
row.names(JKM_df) <- NULL
plot(jkm_daily)
# there is a spike in 2021
JKM_df_copy <- JKM_df
JKM_df_copy[2345:2363,]
JKM_df_copy$price[2348] <- NA
JKM_df_copy$price[2349] <- NA
JKM_df_copy$price[2350] <- NA
JKM_df_copy$price[2351] <- NA
JKM_df_copy$price[2352] <- NA
JKM_df_copy$price[2355] <- NA
JKM_df_copy$price[2356] <- NA
JKM_df_copy$price[2357] <- NA
JKM_df_copy$price[2358] <- NA
JKM_df_copy$price[2358] <- NA
JKM_df_copy$price[2359] <- NA
JKM_df_copy[2340:2363,]

# daily
jkm_daily_ts <- ts(JKM_df_copy$price, start = c(2014,213), frequency = 365) 
jkm_daily <- na.interp(jkm_daily_ts, linear = TRUE)
jkm_daily_log <- log(jkm_daily)
ts_names <- append(ts_names, "jkm_daily")
ts_names <- append(ts_names, "jkm_daily_log")

# weekly
JKM_df_copy$price <- jkm_daily
week <- as.Date(cut(JKM_df_copy$time, "week"))
jkm_weekly <- aggregate(price ~ week,  JKM_df_copy, mean, na.rm=TRUE)
jkm_weekly <- jkm_weekly[-1,]
jkm_weekly <- ts(jkm_weekly$price, start = c(2014, 32), end = c(2022, 44),  frequency = 52)
jkm_weekly_log <- log(jkm_weekly)
ts_names <- append(ts_names, "jkm_weekly")
ts_names <- append(ts_names, "jkm_weekly_log")

# monthly

month <- as.Date(cut(JKM_df_copy$time, "month"))
jkm_monthly <- aggregate(price ~ month,  JKM_df_copy, mean, na.rm=TRUE)
jkm_monthly<- ts(jkm_monthly$price, start = c(2014, 08), frequency = 12 )
jkm_monthly_log <- log(jkm_monthly)
ts_names <- append(ts_names, "jkm_monthly")
ts_names <- append(ts_names, "jkm_monthly_log")


# BRENT  ########################################################

# we will create a copy of this first:
head(ts_og_brent)
ts_og_brent <- ts(eu_brent_sub$price, start = c(1997,  32), frequency = 365)
head(eu_brent_sub)
# now daily
brent_daily <- na.interp(brent_copy, linear = TRUE)
brent_daily_log <- log(brent_daily)
ts_names <- append(ts_names, "brent_daily")
ts_names <- append(ts_names, "brent_daily_log")

# weekly

eu_brent_sub$price <-  brent_daily
week <- as.Date(cut(eu_brent_sub$time, "week"))
brent_weekly <- aggregate(price ~ week,  eu_brent_sub, mean, na.rm=TRUE)
brent_weekly <- ts(brent_weekly$price, start = c(1997, 05), end = c(2022, 47), frequency = 52)
brent_weekly_log <- log(brent_weekly)
ts_names <- append(ts_names, "brent_weekly")
ts_names <- append(ts_names, "brent_weekly_log")

# now we convert to monthly:
month <- as.Date(cut(eu_brent_sub$time, "month"))
brent_monthly <- aggregate(price ~ month,  eu_brent_sub, mean, na.rm=TRUE)
# convert to ts:
brent_monthly <- ts(brent_monthly$price, start = c(1997, 02), frequency = 12 )
brent_monthly_log <- log(brent_monthly)
ts_names <- append(ts_names, "brent_monthly")
ts_names <- append(ts_names, "brent_monthly_log")


# WTI ########################################################

# daily
crude_wti_sub[8400:8500,]
row.names(crude_wti_sub) <- NULL
# negative price on 2020 04 19
crude_wti_sub$price[8479] <- NA
ts_wti_og <- ts(crude_wti_sub$price, start = c(1997, 32), end = c(2022, 331),frequency = 365)
wti_daily <- na.interp(ts_wti_og, linear = TRUE)
wti_daily_log <- log(wti_daily)
ts_names <- append(ts_names, "wti_daily")
ts_names <- append(ts_names, "wti_daily_log")

 # weekly
crude_wti_sub_c <- crude_wti_sub
crude_wti_sub_c$price <- wti_daily
week <- as.Date(cut(crude_wti_sub_c$time, "week"))
wti_weekly <- aggregate(price ~ week,  crude_wti_sub_c, mean, na.rm=TRUE)
wti_weekly <- ts(wti_weekly$price, start=c(1997, 05), end = c(2022, 47), frequency=52)
wti_weekly_log <- log(wti_weekly)
ts_names <- append(ts_names, "wti_weekly")
ts_names <- append(ts_names, "wti_weekly_log")

# monthly

month <- as.Date(cut(crude_wti_sub_c$time, "month"))
wti_monthly <- aggregate(price ~ month,  crude_wti_sub_c, mean, na.rm=TRUE)
wti_monthly <- ts(wti_monthly$price, start=c(1997, 02), frequency=12)
wti_monthly_log <- log(wti_monthly)
ts_names <- append(ts_names, "wti_monthly")
ts_names <- append(ts_names, "wti_monthly_log")

diff(wti_monthly)
wti_monthly
tail(wti_monthly)
tail(diff(wti_monthly))

# EXPORTING #################################################
# creating differenced versions of our time series:
diff_list_ts <- c()
for(i in ts_names){
  ts_type = get(i)
  ts_type_diff = diff(ts_type)
  mts <- as.numeric(time(ts_type_diff))
  tms <- date_decimal(mts)
  diff_ts_name <- paste(i, "diff.csv", sep = "")
  diff_list_ts <- append(diff_list_ts, diff_ts_name)
  write.csv(data.frame(Y=as.matrix(ts_type_diff), date=tms), file=file_name)
  
}


for(i in ts_names){
  ts_type = get(i)
  mts <- as.numeric(time(ts_type))
  tms <- date_decimal(mts)
  file_name <- paste(i, "csv", sep= ".")
  write.csv(data.frame(Y=as.matrix(ts_type), date=tms), file=file_name)
}

for(i in)

  
# for plotting purposes:
  
ttf_plot <- ts(ttf_daily_copy$price, start = c(2010, 3), frequency = 365)
jkm_plot <- ts(JKM_df$price, start = c(2014, 213), frequency = 365)

jkm_plot <- na.interp(jkm_plot, linear = TRUE)
hh_plot <- na.interp(term_ts_hh, linear = TRUE)
nbp_plot <- na.interp(nbp_ts_og, linear = TRUE)
ttf_plot <- na.interp(ttf_plot, linear = TRUE)
# going to plot all graphs
all_ts <- cbind(hh_plot, nbp_plot, ttf_plot, jkm_plot)
t.start <- time(hh_monthly)[220]
window_1 <- window(all_ts, start = t.start)

ts.plot(window_1, gpars = list(col = c(6,2,12,5)), xlim=c(2016,2022.6), ylab="USD/MMBtu")
legend("topleft", legend = c("Henry Hub", "NBP", "TTF", "JKM"), col = c(6,2,12,5), lty = 1)
abline(v = time(hh_monthly)[288], lty = 2,  col = alpha("black", 0.7))   
abline(v = time(hh_monthly)[302], lty = 2, col = alpha("black", 0.7)) 



# now redoing exports and imports:


ng_imports_USA <- ng_imports_USA[-c(1:2),]
ng_imports_USA <- ng_imports_USA[-c(1:288),]
colnames(ng_imports_USA) <- c("date", "price")
ts_ng_imports <- ts(ng_imports_USA$price, start = c(1997,1), frequency = 12 )

ng_exports_USA <- ng_exports_USA[-c(1:2),]
ng_exports_USA <- ng_exports_USA[-c(1:288),]
colnames(ng_exports_USA) <- c("date", "price")
ts_ng_exports <- ts(ng_exports_USA$price, start = c(1997,1), frequency = 12 )


exports_USA <- exports_USA[-c(1:2),]
imports_USA <- imports_USA[-c(1:2),]
colnames(imports_USA) <- c("date", "price")
colnames(exports_USA) <- c("date", "price")
ts_exports <- ts(exports_USA$price, start = c(1997,1), frequency = 12 )
ts_imports <- ts(imports_USA$price, start = c(1997,1), frequency = 12 )

ts_ei <- cbind(ts_ng_exports, ts_ng_imports) 
ts_ei_window <- window(ts_ei, start = time(hh_monthly)[100])
ts.plot(ts_ei, gpars = list(col = c(4,6)), ylab = "MMcf")
legend("topleft", legend = c("Natural Gas exports", "Natural Gas imports"), col = c(4,6), lty = 1)
abline(v = time(hh_monthly)[228], lty = 2, col = alpha("black", 0.7)) 
abline(v = time(hh_monthly)[240], lty = 2, col = alpha("black", 0.7)) 
axis(1, at = c(2016), srt=45, col = "black", las = 1, cex.axis = 0.7)
axis(1, at = c(2017), srt=45, col = "black", las = 1, cex.axis = 0.7)










