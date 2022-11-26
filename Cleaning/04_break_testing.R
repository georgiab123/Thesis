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

NBP_prices <- collated_prices
henry_hub_series <- henry_hub

# DATA CLEANING  ##############################################################

# for NBP prices

NBP_prices <- NBP_prices[-1, ]
NBP_price_series <- NBP_prices[, 2]
 
# let us convert to time series
# we first get the beginning and end dates

date_series <- NBP_prices$Date
last_date <- date_series[1]
first_date <- date_series[length(date_series)]

# let us create a dataframe with just the first two columns:

price_date_NBP <- NBP_prices[,c(1,2)]
colnames(price_date_NBP) <- c("time", "price")

# now we create a continuous time series (continous in the sense of no gaps in 
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
dim(NBP_price_df_COPY)
NBP_price_df_COPY  <-  NBP_price_df_COPY[-9405,]

ts_NBP_prices <- NBP_price_df_COPY$price

term_ts_nbp <- ts(ts_NBP_prices, start=c(1997, 2), end=c(2022, 10), frequency=365)

# now we plot our time series

autoplot(term_ts_nbp) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") + theme_gray() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# CONVERTING DAILY NBP INTO WEEKLY ############################################

week_nbp <- as.Date(cut(NBP_price_df$time, "week"))
weekly_mean_nbp <- aggregate(price ~ week_nbp,  NBP_price_df, mean)

plot_ts_nbp <- ggplot(weekly_mean_nbp, aes(x = week_nbp, y = price))  +
  geom_line(color="steelblue") 

plot_ts_nbp

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

# we remove the first three rows

hh_weekly_online <- hh_weekly_online[-c(1,2,3),]

ts_hh_weekly_online <- ts(hh_weekly_online$price, start=c(1997, 02), end=c(2022, 11), frequency=52)

# the below is a comparison between the online weekly henry hub prices
# and the ones that I have computed by removing the outliers - there is some 
# slight lag as the weekly moving interval is moved by three days in the online 
# version. 

henry_hub_weekly_comp <- cbind(ts_hh_weekly_online, ts_hh_weekly)
colnames(henry_hub_weekly_comp) <- c("Online weekly series", "Computed from daily, two outliers removed")
autoplot(henry_hub_weekly_comp) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))


# BREAKPOINT PLOTTING #########################################################

# We now have a weekly times series for Henry Hub data and for NBP. Let us compute
# the break points for both the weekly time series, stored in :
#  - weekly time series HH: ** henry_hub_weekly_00
#  - daily time series HH: ** hh_prices_01
#  - weekly time series NBP: ** weekly_mean_nbp
#  - daily time series NBP: ** NBP_price_df_COPY


# We will compute the break dates for HH weekly first: 
hh_weekly_03 <- henry_hub_weekly_00[-1,]
ts_weekly_hh <- ts(hh_weekly_03$price, start=c(1997, 02), end=c(2022, 11), frequency=52)
bp_hh_weekly <- breakpoints(ts_weekly_hh  ~ 1)
bp_hh_weekly 
# break points are at index 196 402 617 932 
summary(bp_ts)

# Now we will compute the break dates for HH daily: 
ts_daily_hh <- ts(hh_prices_01$price, start=c(1997, 02), end=c(2022, 11), frequency=365)
bp_hh_daily <- breakpoints(ts_daily_hh   ~ 1)

# compute break points for weekly NBP
weekly_mean_nbp_01 <- weekly_mean_nbp[-1,]
ts_weekly_nbp <- ts(weekly_mean_nbp_01$price, start=c(1997, 02), end=c(2022, 11), frequency=52)
bp_nbp_weekly <- breakpoints(ts_weekly_nbp  ~ 1)

# break points are at index 385, 711, 907, 1114
bp_nbp_weekly


# compute break points for daily NBP


# we add them using the vline function

plot_ts <- ggplot(weekly_mean_hh, aes(x = week, y = price))  +
  geom_line(color="steelblue") + 
  geom_vline(xintercept = as.numeric(weekly_mean_hh$week[195]), linetype = 2) + 
  geom_vline(xintercept = as.numeric(weekly_mean_hh$week[402]), linetype = 2) + 
  geom_vline(xintercept = as.numeric(weekly_mean_hh$week[617]), linetype = 2) + 
  geom_vline(xintercept = as.numeric(weekly_mean_hh$week[932]), linetype = 2) + 
  xlab("")
  
plot_ts

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











