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

# now we insert NA into missing days

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

merged.data$price[which(is.na(merged.data$price))] <- 0
merged.data <- merged.data[-1,]
merged.data <-  merged.data[-9405,]

ts_NBP_prices <- merged.data$price

term_ts_nbp <- ts(ts_NBP_prices, start=c(1997, 2), end=c(2022, 10), frequency=365)

# there are MANY zeros in this data set so the graph is essentially black

# now we plot our time series

autoplot(term_ts_nbp) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") + theme_gray() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# here we look at NBP and Henry Hub prices, looking at breaks in mean/variances

# the following needs to change alot
# convert NBP daily df into weekly df:
NBP_price_df_weekly <- NBP_price_df  %>% 
  mutate(year_month = format(time, "%Y-%V")) %>%
  group_by(year_month) %>%
  summarise(weekly_mean = mean(price, na.rm = TRUE))

NBP_price_df_weekly <- NBP_price_df_weekly[-1,]
NBP_price_df_weekly <- NBP_price_df_weekly[-dim(NBP_price_df_weekly)[1],]

# for some reason there are 53 weeks in this year and not 52 
# so we will delete those entries
# well get back to this lates
NBP_price_df_weekly <- NBP_price_df_weekly[-dim(NBP_price_df_weekly)[1],]
NBP_price_df_weekly <- NBP_price_df_weekly[-dim(NBP_price_df_weekly)[1],]


time.min <- as.Date("1997-01-01")
time.max <- as.Date("2022-11-01")
all.dates <- seq(time.min, time.max, by="week")

check <- cbind(all.dates, NBP_price_df_weekly)
check <- check[,-2]

dim(check)


plot_ts <- ggplot(check, aes(x = all.dates, y = weekly_mean))  +
  geom_line(color="steelblue") #+ 
  #geom_vline(xintercept = as.numeric(weekly_hh$all.dates[195]), linetype = 2) + 
  #geom_vline(xintercept = as.numeric(weekly_hh$all.dates[402]), linetype = 2) + 
  #geom_vline(xintercept = as.numeric(weekly_hh$all.dates[619]), linetype = 2) + 
  #geom_vline(xintercept = as.numeric(weekly_hh$all.dates[934]), linetype = 2) + 
  #xlab("")

plot_ts


# 1348 rows




# HENRY HUB ####################################################################


colnames(henry_hub_series) <- c("time", "price")

# delete the first two rows since they are just NA:
# the following is executed twice: 

henry_hub_series <- henry_hub_series[-1,]

#now we convert to time series and impute NA as zeroes for missing dates: 


# now we insert NA into missing days

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
merged.data <- merged.data[-1,]
hh_prices <- merged.data

#merged.data$price[which(is.na(merged.data$price))] <- 0
#merged.data <- merged.data[-1,]
#merged.data <-  merged.data[-9405,]

ts_HH_prices <- merged.data$price

# we want to start this time series exactly on the second month of 1997 so we will
# eliminate all entries before that time:

term_ts_hh <- ts(ts_HH_prices, start=c(1997, 02), end=c(2022, 11), frequency=365)

autoplot(term_ts_hh) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") + theme_gray() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# there are MANY NA Values in the daily series
# perhaps should use weekly series instead, removing these one off pertubations in the data

# first, we are going to remove the outliers -- there are two significant outliers 
# in this data set we want to remove
 
# we create a copy of our merged data set called hh_prices

hh_prices <- merged.data

# let us remove the below two max prices: 
# or impute them with NA values

# we run the below TWICE which removes our two greatest outliers
# run once
index_max <- which.max(hh_prices$price)
hh_prices[index_max,]$price <- NA

# run twice
index_max <- which.max(hh_prices$price)
hh_prices[index_max,]$price <- NA


# now plot again to show the difference

ts_HH_prices_1 <- hh_prices$price
term_ts_hh_1 <- ts(ts_HH_prices_1, start=c(1997, 02), end=c(2022, 11), frequency=365)

autoplot(term_ts_hh_1) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# GROUPING DAILY DATA BY WEEK #################################################

week <- as.Date(cut(hh_prices$time, "week"))
weekly_mean_hh <- aggregate(price ~ week,  hh_prices_convert, mean)

#now we convert to a time series
ts_hh_weekly <- ts(weekly_mean_hh$price, start=c(1997, 02), end=c(2022, 11), frequency=52)
     
autoplot(ts_hh_weekly) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

# COMPARE TO ONLINE PUBLISHED HENRY HUB PRICES ################################

# let us compare the weekly published henry hub to our found one:

# from local repository, 'hh_weekly'

hh_weekly_O <- hh_weekly

colnames(hh_weekly_O) <- c("time", "price")
hh_weekly_O <- hh_weekly_O[-1,]

ts_hh_weekly_online <- ts(hh_weekly_O$price, start=c(1997, 01), end=c(2022, 11), frequency=52)

# the below is a comparison between the online weekly henry hub prices
# and the ones that I have computed by removing the outliers

henry_hub_weekly_comp <- cbind(ts_hh_weekly_online, ts_hh_weekly)
colnames(henry_hub_weekly_comp) <- c("Computed from daily, outliers removed", "Online weekly series")
autoplot(henry_hub_weekly_comp) + labs(x = "Time", y = "Price, USD/mmBtu") + 
  labs(color="Destination") +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))


# BREAKPOINT PLOTTING #########################################################

# We now have a weekly times series for Henry Hub data. Let us now compute some 
# summary statistics about the Hnry hub:  

# store the breakdates

bp_ts <- breakpoints(ts_hh_weekly ~ 1)
summary(bp_ts)

# we conduct the following purely for plotting purposes
hh_prices_weekly
time.max <- as.Date("2022-12-01")
time.min <- as.Date("1997-01-01")
# we remove the first row of the henry hub data frame:

# the break point indices are 196, 402, 617, 932 corresponding to ts_hh_wekly
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

# lets compare this to NBP prices










