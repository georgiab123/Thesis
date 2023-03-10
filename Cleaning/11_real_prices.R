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










