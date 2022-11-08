# LOADING PACKAGES

library(dplyr)
library(lubridate)
library(ggfortify)
library(stats)
library(forecast)
library(tibbletime)

# IMPORTING DATA

price_data <- collated_prices

# CLEANING #

price_data_r <- price_data [seq(dim(price_data )[1],1),]

# TIME SERIES #

# the below have the same start and end dates
t1 <- ts(price_data_r$us_lng_export_price, start=c(2016, 3), end=c(2022, 8), frequency=12)
t2 <- ts(price_data_r$japan_korea, start=c(2016, 3), end=c(2022, 8), frequency=12)
t3 <- ts(price_data_r$nbp, start=c(2016, 3), end=c(2022, 8), frequency=12)
t4 <- ts(price_data_r$henry_hub, start=c(2016, 3), end=c(2022, 8), frequency=12)

# the below have different start and end dates 
t5 <- ts(price_data_r$dutch_ttf, start=c(2016, 3), end=c(2022, 8), frequency=12)

ts_prices <- cbind(t1,t2,t3,t4,t5)
colnames(ts_prices) <- c("US LNG export prices", " Japan/Korea", "NBP", "Henry Hub", "Dutch TTF")
autoplot(ts_prices, facets = FALSE) +  labs(x = "Time", y = "Monthly Prices, USD/mmBtu") + labs(color="Type")

#  Now just examining LNG export prices and henry hub prices: 
ts_USA <- cbind(t1,t4)
colnames(ts_USA) <- c("US LNG export prices", "Henry Hub")
autoplot(ts_USA, facets = FALSE) + labs(x = "Time", y = "Monthly Prices, USD/mmBtu") + labs(color="Type")

ccf(t1,t4)
print(ccf(t1,t4))

acf(ts_USA, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action = na.fail, demean = TRUE)

pacf(ts_USA)

# We can also compare LNG export prices across terminal: 

LNG_term_prices <- collated_prices
LNG_term_prices <- LNG_term_prices[seq(dim(LNG_term_prices)[1],1),]
LNG_term_prices$freeport <- replace(LNG_term_prices$freeport, LNG_term_prices$freeport == 0, "NA")

term_ts_1 <- ts(LNG_term_prices$sabine_pass, start=c(2016, 3), end=c(2022, 8), frequency=12)
term_ts_2 <- ts(LNG_term_prices$cove_point, start=c(2016, 3), end=c(2022, 8), frequency=12)
term_ts_3 <- ts(LNG_term_prices$corpus_christi, start=c(2016, 3), end=c(2022, 8), frequency=12)
term_ts_4 <- ts(LNG_term_prices$camreron_LA, start=c(2016, 3), end=c(2022, 8), frequency=12)
term_ts_5 <- ts(LNG_term_prices$freeport, start=c(2016, 3), end=c(2022, 8), frequency=12)
term_ts_6 <- ts(LNG_term_prices$elba_island, start=c(2016, 3), end=c(2022, 8), frequency=12)

term_ts_p <- cbind(term_ts_1,term_ts_2,term_ts_3,term_ts_4,term_ts_5,term_ts_6)
colnames(term_ts_p) <- c("Sabine pass", "Cove point", "Corpus Christ", "Cameron, LA", "Freeport", "Elba Island")
autoplot(term_ts_p, facets = FALSE) + labs(x = "Time", y = "Monthly Prices, USD/mmBtu") + labs(color="Type")



     