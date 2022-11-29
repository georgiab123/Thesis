# here we look at NBP and Henry Hub prices, looking at breaks in mean/variances
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

# IMPORTING DATA ##############################################################


# import price series data from local repository

NBP_prices <- NBP_prices
henry_hub_series <- henry_hub
crude_ts <- Crude_WTI_OK
japan_korea <- collated_prices
eu_brent <- EU_BRENT
ttf_ts <- TTF_DUTCH_ALL

# DATA CLEANING  ##############################################################

# for NBP prices, we maintain the first two columns of the data set: 

NBP_prices <- NBP_prices[-1, ]
price_date_NBP <-NBP_prices[,c(1,2)]
colnames(price_date_NBP) <- c("time", "price")

# now we create a continuous time series (continuous in the sense of no gaps in 
# our date column, but gaps in the price series) - we add row entries for the 
# missing days and impute them with NA values 

raw.data <- price_date_NBP
raw.data$time <- anydate(raw.data$time)
raw.data$time <- as.Date(raw.data$time)

sorted.data <- raw.data
data.length <- length(sorted.data$time)

time.max <- sorted.data$time[1]
time.min <- sorted.data$time[data.length]

all.dates <- seq(time.min, time.max, by="day")
all.dates.frame <- data.frame(list(time=all.dates))

merged.data <- merge(all.dates.frame, sorted.data, all=T)
NBP_price_df <- merged.data

# now NBP_price_df is our final df frame

NBP_price_df_COPY <- NBP_price_df 

# we can get the ts to start exactly on the 2nd month of 1997 by deleting the first
# three rows: 

NBP_price_df_COPY  <- NBP_price_df_COPY [-c(1,2,3),]

# we can also get the ts to end exactly on the 10th month by deleting the very last
# entry 

NBP_price_df_COPY  <-  NBP_price_df_COPY[-dim(NBP_price_df_COPY)[1],]

# now convert to ts object: 

ts_NBP_prices <- NBP_price_df_COPY$price
term_ts_nbp <- ts(ts_NBP_prices, start=c(1997, 2), end=c(2022, 10), frequency=365)

# now we plot our time series

autoplot(term_ts_nbp) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") + theme_gray() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# CONVERTING DAILY NBP INTO WEEKLY ############################################

week_nbp <- as.Date(cut(NBP_price_df$time, "week"))
weekly_mean_nbp <- aggregate(price ~ week_nbp,  NBP_price_df, mean)
#check for na values
which(is.na(weekly_mean_nbp))

plot_ts_nbp <- ggplot(weekly_mean_nbp, aes(x = week_nbp, y = price))  +
  geom_line(color="steelblue") 

plot_ts_nbp

# now create a TS:

weekly_mean_nbp
# want to start on 1997-02-03
weekly_mean_nbp <- weekly_mean_nbp[-1,]
NBP_ts <- ts(weekly_mean_nbp$price, start = c(1997,02), end = c(2022,10), frequency = 52)

# HENRY HUB ####################################################################


colnames(henry_hub_series) <- c("time", "price")

# delete the first two rows since they are just NA:

henry_hub_series <- henry_hub_series[-c(1,2),]

# now we add the missing days back in and create NA where they were:

raw.data <- henry_hub_series
raw.data$time <- anydate(raw.data$time)
raw.data$time <- as.Date(raw.data$time)

sorted.data <- raw.data
data.length <- length(sorted.data$time)
time.min <- sorted.data$time[1]
time.max <- sorted.data$time[data.length]

all.dates <- seq(time.min, time.max, by="day")
all.dates.frame <- data.frame(list(time=all.dates))

merged.data <- merge(all.dates.frame, sorted.data, all=T)
hh_prices <- merged.data

# we want to start this time series exactly on the second month of 1997 so we will
# eliminate all entries before that time:

hh_prices_00 <- hh_prices
hh_prices_00 <- hh_prices_00[-c(1:26),]

# we also need to re-index the rownames:

row.names(hh_prices_00) <- NULL

# now we construct the time series: 

ts_HH_prices <- hh_prices_00$price

term_ts_hh <- ts(ts_HH_prices, start=c(1997, 02), end=c(2022, 11), frequency=365)

autoplot(term_ts_hh) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") + theme_gray() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# REMOVING OUTLIERS in daily henry hub ########################################

# first, we are going to remove the outliers -- there are two significant outliers 
# in this data set we want to remove. We create another copy of our merged data 
# set called hh_prices_01, where we use the dataset that starts exactly on the 
# first day of 1997-02-01

hh_prices_01 <- hh_prices_00

# let us remove the below two max prices and impute them with NA values

index_max <- which.max(hh_prices_01$price)
hh_prices_01[index_max,]$price <- NA

# now get the next largest value and set to NA 

index_max <- which.max(hh_prices_01$price)
hh_prices_01[index_max,]$price <- NA

# now plot again to show the difference

ts_HH_prices_01 <- hh_prices_01$price
term_ts_hh_01 <- ts(ts_HH_prices_01, start=c(1997, 02), end=c(2022, 11), frequency=365)

autoplot(term_ts_hh_01) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# GROUPING DAILY DATA BY WEEK #################################################

# we use the daily data with the outliers removed to construct our weekly data: 

week <- as.Date(cut(hh_prices_01$time, "week"))
weekly_mean_hh <- aggregate(price ~ week,  hh_prices_01, mean)

# the above misses the week of 2005-09-26 -- this is because the corresponding
# prices for that week have all NAs in them. We will impute this row and add
# and NA value to it so our time series will correspond better:

new_row <- data.frame(week = as.Date("2005-09-26"), price = NA)
newData <- rbind(weekly_mean_hh[1:452,], new_row, weekly_mean_hh[453:1347,] )
henry_hub_weekly_00 <- newData

#now we convert to a time series
ts_hh_weekly <- ts(henry_hub_weekly_00$price, start=c(1997, 02), end=c(2022, 11), frequency=52)
     
autoplot(ts_hh_weekly) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# COMPARE TO ONLINE PUBLISHED HENRY HUB PRICES ################################

# let us compare the weekly published henry hub to our found one, 
# from local repository, 'hh_weekly'

hh_weekly_online <- hh_weekly

colnames(hh_weekly_online) <- c("time", "price")

# we remove the first two rows since they are missing values / nonsensical

hh_weekly_online <- hh_weekly_online[-c(1,2),]

# we remove the next four rows as they pertain to January

hh_weekly_online <- hh_weekly_online[-c(1,2,3,4),]

# now we create the time series: 

ts_hh_weekly_online <- ts(hh_weekly_online$price, start=c(1997, 02), end=c(2022, 11), frequency=52)

# the below is a comparison between the online weekly henry hub prices
# and the ones that I have computed by removing the outliers - there is some 
# slight lag as the weekly moving interval is moved by three days in the online 
# version. 

henry_hub_weekly_comp <- cbind(ts_hh_weekly_online, ts_hh_weekly)
colnames(henry_hub_weekly_comp) <- c("Online weekly series", "Computed from daily, two outliers removed")
autoplot(henry_hub_weekly_comp) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Type") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# WEEKLY TS HENRY HUB OUTLIERS REMOVED ########################################

# this is the weekly henry hub ultimately used:

plot(hh_weekly_online$time, hh_weekly_online$price, type = "l")
hh_weekly_online_outliers <- hh_weekly_online
hh_weekly_online_outliers[1254,]$price <- (hh_weekly_online_outliers[1253,]$price + hh_weekly_online_outliers[1255,]$price)/2
hh_weekly_online_outliers[317,]$price <- (hh_weekly_online_outliers[316,]$price + hh_weekly_online_outliers[318,]$price)/2
plot(hh_weekly_online_outliers$time,hh_weekly_online_outliers$price, type = "l")


ts_hh_outliers <- ts(hh_weekly_online_outliers$price, start=c(1997, 06), end=c(2022, 50), frequency=52)
plot(ts_hh_outliers)
# CRUDE_WTI  ##################################################################

colnames(crude_ts) <- c("time", "price")
#remove first two rows since both NA values:
crude_ts <- crude_ts[-c(1,2),]
# we want 1997-2 onwards to match Henry Hub price, this is the 580th entry
crude_ts <- crude_ts[-c(1:579),]
# check for NA values -- there are none:
which(is.na(crude_ts$price))
# convert to date formats:
crude_ts$time <- as.Date(crude_ts$time)
# now we have a ts going from 1997-2 to 2022-11

# lets also create a ts with severe outliers removed for the online wekly henry hub
# and the only crude

plot(crude_ts$time, crude_ts$price, type = "l")
which.min(crude_ts$price)
crude_ts[1212,]
crude_ts_outliers <- crude_ts[-1212,]
plot(crude_ts_outliers$time, crude_ts_outliers$price, type = "l")

ts_weekly_online_crude <- ts(crude_ts_outliers$price, start=c(1997, 06), end=c(2022, 50), frequency=52)
plot(ts_weekly_online_crude)
ts_crude_final <- ts_weekly_online_crude 


l# JAPAN_KOREA  #################################################################

japan_korea <- japan_korea[-1,]
japan_korea <- japan_korea[,c(1,2)]
colnames(japan_korea) <- c("time", "price")

# now we correct for missing values in daily time series
raw.data <- japan_korea
raw.data$time <- anydate(raw.data$time)
raw.data$time <- as.Date(raw.data$time)
sorted.data <- raw.data
data.length <- length(sorted.data$time)
time.max <- sorted.data$time[1]
time.min <- sorted.data$time[data.length]
all.dates <- seq(time.min, time.max, by="day")
all.dates.frame <- data.frame(list(time=all.dates))
merged.data <- merge(all.dates.frame, sorted.data, all=T)
japan_korea_df <- merged.data

# now we convert to weekly format:
week <- as.Date(cut(japan_korea_df$time, "week"))
weekly_mean_JK <- aggregate(price ~ week,  japan_korea_df, mean)
which(is.na(weekly_mean_JK))

plot(japan_korea$time, japan_korea$price, type = "l")
plot(weekly_mean_JK[,1], weekly_mean_JK$price, type = "l")

# from the plot, we have significant discontunity from around 2015 onwards.
# we will impte these prices using a mean of the surrounding prices:

weekly_IMPUTED_JK <- weekly_mean_JK
weekly_IMPUTED_JK[15,]$price <- (weekly_IMPUTED_JK[14,]$price + weekly_IMPUTED_JK[17,]$price)/2
weekly_IMPUTED_JK[16,]$price <- (weekly_IMPUTED_JK[15,]$price + weekly_IMPUTED_JK[17,]$price)/2

#now we plot again and see if discontunuity is removed:

plot(jk_wk[,1], jk_wk$price, type = "l")

# now we create a ts
# remove first row so we start exactly in the 10th month of 2012
# we will look at 2014 onwards
weekly_IMPUTED_JK_REMOVED <- weekly_IMPUTED_JK
weekly_IMPUTED_JK_REMOVED <- weekly_IMPUTED_JK_REMOVED[-1,]
jk_wk <- weekly_IMPUTED_JK_REMOVED
jk_wk <- jk_wk[-c(1:16),]

JK_ts <- ts(jk_wk $price, start = c(2014, 32), end = c(2022, 46), frequency = 52)
plot(JK_ts)

# EUROPEAN BRENT ##############################################################

eu_brent <- eu_brent[-c(1,2),]
colnames(eu_brent) <- c("time", "price")

# we just want a subset from 1997 - 02 onwards: 
eu_brent_sub <- eu_brent[-c(1:508),]

# plot and check for significant outliers:
plot(eu_brent_sub$time, eu_brent_sub$price, type = "l")
#nothing too severe, convert to ts:

eu_brent_ts <- ts(eu_brent_sub$price, start = c(1997,05), end = c(2022, 50), frequency =52)
plot(eu_brent_ts)

# DUTCH TTF ####################################################################

# remove multiple first rows as they are just NA values:

ttf_ts <- ttf_ts[-c(1:17),]
# keep only first two rows:
ttf_ts <- ttf_ts[, c(1,2)]
colnames(ttf_ts) <- c("time", "price")

# now impute all daily values: 
raw.data <- ttf_ts
raw.data$time <- anydate(raw.data$time)
raw.data$time <- as.Date(raw.data$time)
sorted.data <- raw.data
data.length <- length(sorted.data$time)
time.max <- sorted.data$time[1]
time.min <- sorted.data$time[data.length]
all.dates <- seq(time.min, time.max, by="day")
all.dates.frame <- data.frame(list(time=all.dates))
merged.data <- merge(all.dates.frame, sorted.data, all=T)
ttf_ts_df <- merged.data

# all these prices are in USD/Megawatt hour [MWh]
# we convert to mibblion BTu by dividing by 3.412141
ttf_ts_df$price <- (ttf_ts_df$price)/3.412141633

week <- as.Date(cut(ttf_ts_df$time, "week"))
weekly_mean_ttf <- aggregate(price ~ week,  ttf_ts_df , mean)
which(is.na(weekly_mean_ttf))

plot(weekly_mean_ttf[,1], weekly_mean_ttf$price, type = "l")

# now convert to a time series object
# remove first tow so starts exactly in 2010 - 01
weekly_mean_ttf <- weekly_mean_ttf[-1,]
ttf_dutch_ts <- ts(weekly_mean_ttf$price, start = c(2010, 1), end = c(2022, 46), frequency = 52)
plot(ttf_dutch_ts)

# now we plot 

# BREAKPOINT PLOTTING ##########################################################

# We now have a weekly times series for Henry Hub data and for NBP. Let us compute
# the break points for both the weekly time series, stored in :
#  - weekly time series HH: ** henry_hub_weekly_00
#  - daily time series HH: ** hh_prices_01
#  - weekly time series NBP: ** weekly_mean_nbp
#  - daily time series NBP: ** NBP_price_df_COPY

# we need to omit an NA value, which is the 452 value for the below to work: 

# We will compute the break dates for HH weekly first: 
# we get rid of the first row since it is in the month of January

hh_weekly_03 <- henry_hub_weekly_00[-1,]

# we need to omit an NA value, so take the average of surround prices and impute: 

which(is.na(ts_weekly_hh))
avg_price_imput <- (hh_weekly_03[451,]$price + hh_weekly_03[453,]$price)/2
hh_weekly_03[452,]$price <- avg_price_imput 

# now w convert to ts:
ts_weekly_hh <- ts(hh_weekly_03$price, start=c(1997, 02), end=c(2022, 11), frequency=52)

# we compute breakpoints without any transformation
bp_hh_weekly <- breakpoints(ts_weekly_hh  ~ 1, format.times = TRUE)

# break points are at index 196 402 617 932 
summary(bp_hh_weekly)

# compute breakdates corresponding to the
# breakpoints of minimum BIC segmentation
breakdates(bp_hh_weekly)
## confidence intervals
ci.hh <- confint(bp_hh_weekly)
ci.hh

#below functionality shows breakpoints and CI for them: 
plot(ts_weekly_hh,  xlab="Time", ylab="USD/ MMBtu")
lines(ci.hh)


#BIVARIATE BREAK TESTING -- FIRST EXAMPLE ######################################

# let's check HH against WTI crude: 
# we will use the online HH for this, since its dates match perfectly: 

dim(hh_weekly_online)
dim(crude_ts)

# now that we have converted to ts, they have the same length. 
length(ts_weekly_online_hh)
length(ts_weekly_online_crude)

# check for NA values -- no NA values: 

which(is.na(ts_weekly_online_hh))
which(is.na(ts_weekly_online_crude))

# now we do break testing (on the online time series with outliers imputed) 
# from df hh_weekly_online_outliers 

ts_crude_final

bp_crude_hh <- breakpoints(ts_hh_outliers ~ ts_crude_final)
bp_crude_hh
ci.crude_hh <- confint(bp_crude_hh)

# now we plot with CI and breakpoints
par(mar=c(5,4,4,5)+.1)
plot(ts_weekly_online_crude,  xlab="Time", ylab="Price", col = "blue")
par(new=TRUE)
plot(ts_hh_outliers,  xlab="Time", ylab="Dollars per Barrel", col = "black", axes = FALSE)
axis(4)
mtext("USD/MMBtu", side=4, line=3)
lines(ci.crude_hh)
legend('topright', legend=c("Crude, WTI: Dollars per Barrel", "Henry Hub: USD/MMBtu"),
       col=c("blue", "black"), lty=1, cex=0.8)

# Now, lets do break testing on an ECM model: 

# first, we test whether these time series are stationary: 


adf.test(ts_hh_outliers)
adf.test(ts_crude_final)

# they are not stationary

# so we need to take the first difference of both of these time series:

ts_hh_outliers_diff <- diff(ts_hh_outliers)
ts_hh_crude_diffed <- diff(ts_crude_final) 

# lets check whether they are stationary now -- they are both stationary
# and of order one! 

adf.test(ts_hh_outliers_diff)
adf.test(ts_hh_crude_diffed)

# Now we construct the ECM:

coint.res_1 <- residuals(lm(ts_hh_outliers ~ ts_crude_final))
coint.res <- stats::lag(ts(coint.res_1, start = c(1997,06), freq = 52), k = -1)
# now we change the window of question we are looking at: 
relations <- cbind(ts_hh_outliers_diff, ts_hh_crude_diffed, ts_hh_outliers, ts_crude_final, coint.res)
relations_wind <- window(relations, start = c(1997,07), end = c(2022, 50))
colnames(relations_wind) <- c("diff.hh", "diff.crude", "hh", "crude", "coint.res1")

ecm.model <- diff.hh ~ coint.res1 + diff.crude 
bp.ecm <- breakpoints(diff.hh ~ coint.res1 + diff.crude, data = relations_wind)
bp2 <-  breakpoints(ts(coint.res_1, start = c(1997,07), freq = 52) ~ 1)
ci.ecm <- confint(bp.ecm)
ci2 <- confint(bp2)

# it should be first differencing really
par(mar=c(5,4,4,5)+.1)
plot(ts_weekly_online_crude ,  xlab="Time", ylab="Dollars per Barrel", col = "blue")
par(new=TRUE)
plot(ts_hh_outliers, xlab="Time", ylab="Dollars per Barrel", col = "black", axes = FALSE)
axis(4)
mtext("USD/MMBtu", side=4, line=3)
lines(ci.ecm)
legend('topleft', legend=c("Crude, WTI: Dollars per Barrel", "Henry Hub: USD/MMBtu"),
       col=c("blue", "black"), lty=1, cex=0.8)


# other statistics
ocus <- efp(ecm.model, type="OLS-CUSUM", data = relations_wind)
me <- efp(ecm.model, type="ME", data=relations_wind, h=0.2)
ocus
bound.ocus <- boundary(ocus, alpha=0.05)
plot(ocus)
plot(me, functional = NULL) 
sctest(ocus)
fs <- Fstats(ecm.model, data = relations_wind)
fs
plot(fs)
plot(fs, aveF=TRUE)
plot(fs, pval=TRUE)
sctest(fs, type="expF")

# enumerate all possible break dates

# now can do for NBP, for european BRENT, for Japan/Korea, but not for Dutch TTF:

# ALL DATA SETS ################################################################

# hh weekly ts: ts_hh_outliers
# WTI OK ts: ts_crude_final
# NBP ts: NBP_ts 
# JK ts: JK_ts 
# TTF:  ttf_dutch_ts
# European Brent: eu_brent_ts 
# coal prices

# let us test the stationarity of all these ts: 

adf.test(ts_hh_outliers)
adf.test(ts_crude_final) 
adf.test(NBP_ts)
adf.test(JK_ts)
adf.test(eu_brent_ts)
adf.test(ttf_dutch_ts)

# now let us first difference these:

hh_ts_diff <- diff(ts_hh_outliers)
crude_ts_diff <- diff(ts_weekly_online_crude)
nbp_ts_diff <- diff(NBP_ts)
jk_ts_diff <- diff(JK_ts)
brent_ts_diff <- diff(eu_brent_ts) 
ttf_ts_diff <- diff(ttf_dutch_ts)

# now let us test again -- the below are all stationary

adf.test(hh_ts_diff) 
adf.test(crude_ts_diff)  
adf.test(nbp_ts_diff) 
adf.test(jk_ts_diff)
adf.test(brent_ts_diff)
adf.test(ttf_ts_diff)

# test for co-integration:
jotest=ca.jo(data.frame(ts_hh_outliers,ts_weekly_online_crude) , type="trace", K=2, ecdet="none", spec="longrun")
summary(jotest)


# now can construct an ECM for each one: 

# eu brent and hh weekly:
length(eu_brent_ts)
length(ts_hh_outliers)

# start eu_brent_ts one month later:
eu_brent_ts_1 <- window(eu_brent_ts, start = c(1997, 6))
brent_ts_diff_1 <- diff(eu_brent_ts_1) 

# now we can conduct an ecm on our series!

coint.res_1 <- residuals(lm(ts_hh_outliers ~ eu_brent_ts_1))
coint.res <- stats::lag(ts(coint.res_1, start = c(1997,06), freq = 52), k = -1)
# now we change the window of question we are looking at: 
relations <- cbind(ts_hh_outliers_diff, brent_ts_diff_1, ts_hh_outliers, eu_brent_ts_1, coint.res)
relations_wind <- window(relations, start = c(1997,07), end = c(2022, 50))
colnames(relations_wind) <- c("diff.hh", "diff.brent", "hh", "crude", "coint.res1")

# estimate breakpoints in long run equilibrium equation
ecm.model <- diff.hh ~ coint.res1 + diff.brent
bp.ecm <- breakpoints(ts_hh_outliers ~ eu_brent_ts_1 + 1)
ci.ecm <- confint(bp.ecm)

# it should be first differencing really
par(mar=c(5,4,4,5)+.1)
plot(eu_brent_ts_1 ,  xlab="Time", ylab="Dollars per Barrel", col = "blue")
par(new=TRUE)
plot(ts_hh_outliers, xlab="Time", ylab="Dollars per Barrel", col = "black", axes = FALSE)
axis(4)
mtext("USD/MMBtu", side=4, line=3)
lines(ci.ecm)
legend('topleft', legend=c("Eu Brent: Dollars per Barrel", "Henry Hub: USD/MMBtu"),
       col=c("blue", "black"), lty=1, cex=0.8)



# BELOW IS REDUNDANT ###########################################################

# change in co-integrating vector
# test for co-integration 
# manually impute the 
# run an ECM without ECM without E
# break test of co-integaating vectors

# breaks in co-integrating vectors
# regressing y on x, co-integrating 
# what if there has been 
# what if there is disparate co-intgeration
# breaking co-integrating co-efficients
# 1992 t racing - hensen
# google scholar 

###############################################################################

# Now we will compute the break dates for HH daily: 
ts_daily_hh <- ts(hh_prices_01$price, start=c(1997, 02), end=c(2022, 11), frequency=365)
bp_hh_daily <- breakpoints(ts_daily_hh   ~ 1)

# compute break points for weekly NBP
weekly_mean_nbp_01 <- weekly_mean_nbp[-1,]
ts_weekly_nbp <- ts(weekly_mean_nbp_01$price, start=c(1997, 02), end=c(2022, 11), frequency=52)
bp_nbp_weekly <- breakpoints(ts_weekly_nbp  ~ 1)

# break points are at index 385, 711, 907, 1114
bp_nbp_weekly


# now need to deduce what type of test  is being used here
# another method of visualisation below: 

ts_hh_weekly_TEST <- ts_hh_weekly
bp_ts <- breakpoints(ts_hh_weekly_TEST ~ 1)
summary(bp_ts)

# fit segmented model with two breaks from minimized BIC
fac.ri <- breakfactor(bp_ts, breaks = 5, label = "seg")

# remove first and last entry of ts_hh_weekly:
ts_hh_weekly_TEST  <- ts_hh_weekly_TEST[-1]
ts_hh_weekly_TEST <- ts_hh_weekly_TEST[-length(ts_hh_weekly_TEST)]

fm.ri <- lm(ts_hh_weekly_TEST ~ 0 + fac.ri)
summary(fm.ri)

## Visualization
plot(ts_hh_weekly_TEST, type = 'l')
lines(fitted(fm.ri), col = 2)

# we have calculated five breaks in the above time series

# BIVARIATE BREAK TESTING ######################################################
# lets compare this to NBP prices

# let us make sure we have the right NBP prices first:

# the weekly time series for NBP is *weekly_mean_nbp*
# let us convert into a time series:

# DATE CHECKING ###############################################################
# the last date is 2022-09-26
tail(weekly_mean_nbp, 1)
# the first date is 1997-01-27
head(weekly_mean_nbp, 1)
# 

# now for henry hub, the last date is 2022-11-21:
tail(weekly_mean_hh, 1)
# while the first recorded date is 1997-02-03: 
head (weekly_mean_hh, 1)

# we will need to re-concile these recorded dates - we take away one of the latest
# entries from henry hub since it is slightly ahead. 
# entry from nbp since it is recorded one week ahead. 
dim(weekly_mean_hh)
dim(weekly_mean_nbp)

hh_weekly_BREAK <- weekly_mean_hh[-dim(weekly_mean_hh)[1],]

# the dates are not aligning between these datasets- that is, there is the same
# amount of entries but differing dates attached to them. Let us examine this:
dates_check <- cbind(hh_weekly_BREAK, weekly_mean_nbp)
dates_check <- dates_check[,-2]
dates_check <- dates_check[,-3]
colnames(dates_check) <- c('hh_week', 'nbp_week')
head(dates_check)

# we will shuffle up nbp week by one entry, and delete a later entry of henry hub
# create copies

weekly_mean_nbp_0 <- weekly_mean_nbp
hh_weekly_BREAK_0 <- hh_weekly_BREAK

hh_weekly_BREAK_0 <- hh_weekly_BREAK_0[-dim(hh_weekly_BREAK_0)[1],]
weekly_mean_nbp_0 <- weekly_mean_nbp_0[-1,]
dates_check_0 <- cbind(hh_weekly_BREAK_0, weekly_mean_nbp_0)
dates_check_0 <- dates_check_0[,-2]
dates_check_0 <- dates_check_0[,-3]
colnames(dates_check_0) <- c('hh_week', 'nbp_week')
head(dates_check_0)
tail(dates_check_0)

# after the 451st entry, the dates diverge
length(which(dates_check_0$hh_week == dates_check_0$nbp_week))
dates_check_0[c(450,451, 452, 453),]

# henry hub data frame has missing 2005-09-26 entry -- why? 
 
# the weekly time series for Henry Hub

ts_hh_weekly <- ts(weekly_mean_hh$price, start=c(1997, 02), end=c(2022, 11), frequency=52)











