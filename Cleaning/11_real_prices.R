# LOADING PACKAGES

install.packages("priceR")
library(priceR)
library(quantmod)
library(rccdates)

# CONVERTING TO REAL DATA


# Getting yearly CPI data from FRED
getSymbols("CPIAUCSL", src='FRED') #Consumer Price Index for All Urban Consumers: All Items
set.seed(1)
p <- xts(rnorm(73, mean=10, sd=3), seq(from=as.Date('1997-12-01'), by='years', length.out=73))
colnames(p) <- "price"
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- avg.cpi/as.numeric(avg.cpi['2022']) #using 2008 as the base year
dat <- merge(p, cf, all=FALSE)
dat$adj <- dat[, 1] * dat[, 2]
tail(dat)
dat$year <- format(as.Date(time(dat$CPIAUCSL), format="%Y-%m-%d"),"%Y")
CPI_year <- cbind(dat$CPIAUCSL, dat$year)
colnames(CPI_year) <- c("pi", "year")

# pi contains price index, year  contains the year 
CPI_year 

# HH daily data 
hh_daily_data <- data.frame(cbind(hh_daily, floor(time(hh_daily))))
colnames(hh_daily_data) <-  c("price", "year")
head(hh_daily_data)
hh_daily_data["cpi_year"] <-  c(1:length(hh_daily))

for(i in CPI_year$year){
    hh_daily_data$cpi_year[hh_daily_data$year == i] <- CPI_year$pi[CPI_year$year == i]
}

# adj contained adjusted prices
hh_daily_data$adj <- hh_daily_data$price / hh_daily_data$cpi_year

hh_daily_real <- hh_daily_data$adj 

# plot to show the difference between adjusted prices and nominal prices
plot(time(hh_daily), hh_daily_data$adj, type = "l", ylim = c(0, 25))
par(new = "TRUE")
plot(time(hh_daily), hh_daily, type = "l", col = "red", ylim = c(0, 25))

# BRENT , WTI, NBP, TTF, JKM 
brent_daily_data <- data.frame(cbind(brent_daily, floor(time(brent_daily))))
wti_daily_data <- data.frame(cbind(wti_daily, floor(time(wti_daily))))
nbp_daily_data <- data.frame(cbind(nbp_daily, floor(time(nbp_daily))))
ttf_daily_data <- data.frame(cbind(ttf_daily, floor(time(ttf_daily))))
jkm_daily_data <- data.frame(cbind(jkm_daily, floor(time(jkm_daily))))
colnames(brent_daily_data) <-  c("price", "year")
colnames(wti_daily_data) <-  c("price", "year")
colnames(nbp_daily_data) <-  c("price", "year")
colnames(ttf_daily_data) <-  c("price", "year")
colnames(jkm_daily_data) <-  c("price", "year")
brent_daily_data["cpi_year"] <-  c(1:length(brent_daily))
wti_daily_data["cpi_year"] <-  c(1:length(wti_daily))
nbp_daily_data["cpi_year"] <-  c(1:length(nbp_daily))
ttf_daily_data["cpi_year"] <-  c(1:length(ttf_daily))
jkm_daily_data["cpi_year"] <-  c(1:length(jkm_daily))

for(i in CPI_year$year){
  brent_daily_data$cpi_year[brent_daily_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  wti_daily_data$cpi_year[wti_daily_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  nbp_daily_data$cpi_year[nbp_daily_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  ttf_daily_data$cpi_year[ttf_daily_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  jkm_daily_data$cpi_year[jkm_daily_data$year == i] <- CPI_year$pi[CPI_year$year == i]
}

# adj contained adjusted prices
brent_daily_data$adj <- brent_daily_data$price / brent_daily_data$cpi_year
wti_daily_data$adj <- wti_daily_data$price / wti_daily_data$cpi_year
nbp_daily_data$adj <- nbp_daily_data$price / nbp_daily_data$cpi_year
ttf_daily_data$adj <- ttf_daily_data$price / ttf_daily_data$cpi_year
jkm_daily_data$adj <- jkm_daily_data$price / jkm_daily_data$cpi_year

brent_daily_real <- brent_daily_data$adj 
wti_daily_real <- wti_daily_data$adj 
nbp_daily_real <- nbp_daily_data$adj 
ttf_daily_real <- ttf_daily_data$adj 
jkm_daily_real <- jkm_daily_data$adj 

# plot to show the difference between adjusted prices and nominal prices
plot(time(brent_daily), brent_daily_real, type = "l", ylim = c(0, 200))
par(new = "TRUE")
plot(time(brent_daily), brent_daily, type = "l", col = "red", ylim = c(0, 200))

# now we export each of these as csvs:
# first levels 

write.csv(hh_daily_real,    file="hh_real.csv")
write.csv(brent_daily_real, file="brent_real.csv")
write.csv(wti_daily_real,   file="wti_real.csv")
write.csv(nbp_daily_real,   file="nbp_real.csv")
write.csv(ttf_daily_real,   file="ttf_real.csv")
write.csv(jkm_daily_real,   file="jkm_real.csv")

hh_daily_real <- ts(hh_daily_real, start = c(1997, 32), frequency = 365)
brent_daily_real <- ts(brent_daily_real, start = c(1997, 32), frequency = 365)
wti_daily_real <- ts(wti_daily_real, start = c(1997, 32), frequency = 365)
nbp_daily_real <- ts(nbp_daily_real, start = c(1997, 32), frequency = 365)
ttf_daily_real <- ts(ttf_daily_real, start =  c(2010, 03), frequency = 365)
jkm_daily_real <- ts(jkm_daily_real, start = c(2014, 213), frequency = 365)


# CONVERTING WORLD BANK DATA AND MONTHLY HH DATA TO REAL PRICES ##############

hh_monthly
wb_japan_lng
wb_eu_ng

hh_monthly_real <- hh_monthly
wb_japan_real <- wb_japan_lng
wb_eu_real <- wb_eu_ng

ttf_monthly_real <- ttf_monthly
nbp_monthly_real <- nbp_monthly
jkm_monthly_real <- jkm_monthly
wti_monthly_real <- wti_monthly
brent_monthly_real <- brent_monthly

  
  
hh_monthly_data <- data.frame(cbind(hh_monthly, floor(time(hh_monthly))))
wb_jap_real_data <- data.frame(cbind(wb_japan_real, floor(time(wb_japan_real))))
wb_eu_real_data <- data.frame(cbind(wb_eu_real, floor(time(wb_eu_real))))
ttf_monthly_data <- data.frame(cbind(ttf_monthly, floor(time(ttf_monthly))))
nbp_monthly_data <- data.frame(cbind(nbp_monthly, floor(time(nbp_monthly))))
jkm_monthly_data <- data.frame(cbind(jkm_monthly, floor(time(jkm_monthly))))
wti_monthly_data <- data.frame(cbind(wti_monthly, floor(time(wti_monthly))))
brent_monthly_data <- data.frame(cbind(brent_monthly, floor(time(brent_monthly))))

colnames(hh_monthly_data) <- c("price", "year")
colnames(wb_jap_real_data) <- c("price", "year")
colnames(wb_eu_real_data) <- c("price", "year")
colnames(ttf_monthly_data) <- c("price", "year")
colnames(nbp_monthly_data) <- c("price", "year")
colnames(jkm_monthly_data) <- c("price", "year")
colnames(wti_monthly_data) <- c("price", "year")
colnames(brent_monthly_data) <- c("price", "year")

for(i in CPI_year$year){
  hh_monthly_data$cpi_year[hh_monthly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  wb_jap_real_data$cpi_year[wb_jap_real_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  wb_eu_real_data$cpi_year[wb_eu_real_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  
  ttf_monthly_data$cpi_year[ttf_monthly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  nbp_monthly_data$cpi_year[nbp_monthly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  jkm_monthly_data$cpi_year[jkm_monthly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  wti_monthly_data$cpi_year[wti_monthly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  brent_monthly_data$cpi_year[brent_monthly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
}


# adj contained adjusted prices
hh_monthly_data$adj <- hh_monthly_data$price / hh_monthly_data$cpi_year
wb_jap_real_data$adj <- wb_jap_real_data$price / wb_jap_real_data$cpi_year
wb_eu_real_data$adj <- wb_eu_real_data$price / wb_eu_real_data$cpi_year

ttf_monthly_data$adj <- ttf_monthly_data$price / ttf_monthly_data$cpi_year
nbp_monthly_data$adj <- nbp_monthly_data$price / nbp_monthly_data$cpi_year
jkm_monthly_data$adj <- jkm_monthly_data$price / jkm_monthly_data$cpi_year
wti_monthly_data$adj <- wti_monthly_data$price / wti_monthly_data$cpi_year
brent_monthly_data$adj <- brent_monthly_data$price / brent_monthly_data$cpi_year

hh_monthly_real <- hh_monthly_data$adj 
wb_japan_real <- wb_jap_real_data$adj
wb_eu_real <- wb_eu_real_data$adj

ttf_monthly_real <- ttf_monthly_data$adj 
nbp_monthly_real <- nbp_monthly_data$adj 
jkm_monthly_real <- jkm_monthly_data$adj 
wti_monthly_real <- wti_monthly_data$adj 
brent_monthly_real <- brent_monthly_data$adj 


# ends november 2022
hh_monthly_real <- ts(hh_monthly_real, start = c(1997, 2), frequency = 12)
# ends september 2022
wb_japan_real  <- ts(wb_japan_real , start = c(1997, 2), frequency = 12)
wb_eu_real <- ts(wb_eu_real, start = c(1997, 2), frequency = 12)

ttf_monthly_real <- ts(ttf_monthly_real, start = c(2010, 1), frequency = 12)
nbp_monthly_real <- ts(nbp_monthly_real, start = c(1997, 2), frequency = 12)
jkm_monthly_real <- ts(jkm_monthly_real, start = c(2014, ), frequency = 12)
wti_monthly_real <- ts(wti_monthly_real, start = c(1997, 2), frequency = 12)
brent_monthly_real <- ts(brent_monthly_real, start = c(1997, 2), frequency = 12)



cut_hh <- ts(hh_monthly_real[c(1:308)] , start = c(1997, 2), frequency = 12)

plot(wb_japan_real, ylim = c(0, 25), col  = "red")
par(new = "TRUE")
plot(cut_hh, type = "l", ylim = c(0, 25))


# writing to csv
write.csv(cut_hh,    file="hh_monthly_real.csv")
write.csv(wb_japan_real, file="jap_real.csv")


# LNG EXPORTS #########################################################

lng_exports

us_exports <- as.data.frame(lng_exports)
colnames(us_exports) <- c("price")
us_exports$rev <- rev(us_exports$price)

lng_us <- ts(us_exports$price, start = c(2016,2), frequency = 12)


# WEEKLY DATA #########################################################

hh_weekly_data <- data.frame(cbind(hh_weekly, floor(time(hh_weekly))))
ttf_weekly_data <- data.frame(cbind(ttf_weekly, floor(time(ttf_weekly))))
nbp_weekly_data <- data.frame(cbind(nbp_weekly, floor(time(nbp_weekly))))
jkm_weekly_data <- data.frame(cbind(jkm_weekly, floor(time(jkm_weekly))))
wti_weekly_data <- data.frame(cbind(wti_weekly, floor(time(wti_weekly))))
brent_weekly_data <- data.frame(cbind(brent_weekly, floor(time(brent_weekly))))

colnames(hh_weekly_data) <- c("price", "year")
colnames(ttf_weekly_data) <- c("price", "year")
colnames(nbp_weekly_data) <- c("price", "year")
colnames(jkm_weekly_data) <- c("price", "year")
colnames(wti_weekly_data) <- c("price", "year")
colnames(brent_weekly_data) <- c("price", "year")

for(i in CPI_year$year){
  hh_weekly_data$cpi_year[hh_weekly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  ttf_weekly_data$cpi_year[ttf_weekly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  nbp_weekly_data$cpi_year[nbp_weekly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  jkm_weekly_data$cpi_year[jkm_weekly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  wti_weekly_data$cpi_year[wti_weekly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
  brent_weekly_data$cpi_year[brent_weekly_data$year == i] <- CPI_year$pi[CPI_year$year == i]
}


# adj contained adjusted prices
hh_weekly_data$adj <- hh_weekly_data$price / hh_weekly_data$cpi_year
ttf_weekly_data$adj <- ttf_weekly_data$price / ttf_weekly_data$cpi_year
nbp_weekly_data$adj <- nbp_weekly_data$price / nbp_weekly_data$cpi_year
jkm_weekly_data$adj <- jkm_weekly_data$price / jkm_weekly_data$cpi_year
wti_weekly_data$adj <- wti_weekly_data$price / wti_weekly_data$cpi_year
brent_weekly_data$adj <- brent_weekly_data$price / brent_weekly_data$cpi_year

hh_weekly_real <- hh_weekly_data$adj 
ttf_weekly_real <- ttf_weekly_data$adj 
nbp_weekly_real <- nbp_weekly_data$adj 
jkm_weekly_real <- jkm_weekly_data$adj 
wti_weekly_real <- wti_weekly_data$adj 
brent_weekly_real <- brent_weekly_data$adj 

# convert to time series format:

hh_weekly_real <- ts(hh_weekly_real,  start=c(1997, 05), end = c(2022, 47), frequency=52)
ttf_weekly_real <- ts(ttf_weekly_real,  start = c(2010, 1), end = c(2022, 46), frequency = 52)
nbp_weekly_real <- ts(nbp_weekly_real, start = c(1997,02), frequency = 52)
jkm_weekly_real <- ts(jkm_weekly_real,start = c(2014, 32), end = c(2022, 44),  frequency = 52)
wti_weekly_real <- ts(wti_weekly_real,start=c(1997, 05), end = c(2022, 47), frequency=52)
brent_weekly_real <- ts(brent_weekly_real, start = c(1997, 05), end = c(2022, 47), frequency = 52)

# write to a csv file 
write.csv(hh_weekly_real,    file="hh_w_real.csv")
write.csv(ttf_weekly_real,    file="ttf_w_real.csv")
write.csv(nbp_weekly_real,    file="nbp_w_real.csv")
write.csv(jkm_weekly_real,    file="jkm_w_real.csv")
write.csv(wti_weekly_real,    file="wti_w_real.csv")
write.csv(brent_weekly_real,    file="brent_w_real.csv")








