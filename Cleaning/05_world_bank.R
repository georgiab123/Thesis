# importing libraries

library(stringr)

# importing data
# from local repo
world_bank <- Word_bank_data

# importing

# only concerned with japanese LNG price data rn: 
# but need to convert data column to correct column: 

vector_names <- str_split(world_bank$Date, "M")
v_names <- unlist(vector_names)
date_v <- c()
months <-c()
years <- c()
j = 1 
for(i in 1:length(v_names)){
  if((i %% 2) == 0) {
    months <- append(months, v_names[i])
  } 
  if((i %% 2) != 0){
    years <- append(years, v_names[i])
  }
}
for(i in 1:length(years)){
  date <- paste(years[i], sep = "-", months[i])
  date_v <- append(date_v, date)
}

world_bank$Date <- date_v

world_bank$Date <- anydate(world_bank$Date, "%Y-%m")

colnames(world_bank) <- c("time", "avg_crude", "brent_crude", "dubai_crude", "wti_crude", "aus_coal", "sa_coal", "us_gas", "eu_gas", "jap_gas")
# now lets plot the japanese monthly:

JK_ts <- ts(jk_wk$price, start = c(2015, 1),  frequency = 52)
plot(world_bank$time[-c(1:456)], world_bank$jap_gas[-c(1:456)], type = "l", col = "red", xlab = "Date", ylab = "US dollars per MMBtu")
par(new=TRUE)
plot(jk_wk$week[-c(1:22)], jk_wk$price[-c(1:22)], col = "blue", axes = FALSE, type = "l", xlab = "", ylab = "")
legend('topleft', legend=c("World bank: monthy Japanese LNG", "S&P Platts: weekly Japanese LNG, outliers removed"),
       col=c("red", "blue"), lty=1, cex=0.8)

#2012-09-01
plot(monthly_mean_JK[,1], monthly_mean_JK[,2], type = "l")
par(new=TRUE)
# want world bank data to start at 2012-09-01
plot(world_bank$time[-c(1:428)], world_bank$jap_gas[-c(1:428)], type = "l", col = "red", xlab = "Date", ylab = "US dollars per MMBtu")

# now we plot 2014 onwards since there is too much discontunuity before 2014:
# 8   2014-07-01 11.000000[]
# first check the lengths:
length(monthly_mean_JK[,1][-c(1:7)])
rownames(monthly_mean_JK) <- NULL
length(world_bank$time[-c(1:450)])

# 
subset_JKM <- monthly_mean_JK[-c(1:7),]
rownames(subset_JKM) <- NULL
subset_JKM <- subset_JKM[-c(dim(subset_JKM)[1]),]

length(subset_JKM [,1])
length(world_bank$time[-c(1:450)])
#
#no outliers removed from the below: 
plot(subset_JKM[,1], subset_JKM[,2], type = "l", axes =  TRUE, xlab = "", ylab = "", col = "blue", ylim = c(1,60))
par(new = TRUE)

plot(world_bank$time[-c(1:241)], world_bank$jap_gas[-c(1:241)], type = "l", col = "red", xlab = "Date", ylab = "US dollars per MMBtu", , ylim = c(1,60))
legend('topleft', legend=c("World bank: monthy Japanese LNG", "S&P Platts: monthly JKM"),
       col=c("red", "blue"), lty=1, cex=0.8)
par(new = TRUE)
plot(wb_japan_lng, ylim  = c(1,60))

wb_japan_lng <- world_bank[-c(1:241),]
wb_japan_lng <- wb_japan_lng$jap_gas
plot(wb_japan_lng)
wb_japan_lng <- ts(wb_japan_lng, start = c(1997, 02), frequency = 12)
plot(wb_japan_lng)

# lets also get the european natural gas prices:
wb_eu_ng <- world_bank[-c(1:241),]
wb_eu_ng <- wb_eu_ng$eu_gas
wb_eu_ng <- ts(wb_eu_ng, start = c(1997, 02), end = c(2022,09), frequency = 12)

ts_graph <- ts(ts_month_ttf, end =  c(2022, 09), frequency = 12)
test <- cbind(wb_eu_ng , ts_month_ttf)
ts_graph <-  ts(test[,2], start = c(1997,02), end = c(2022,09), frequency = 12)

ts_month_nbp_graph <-  ts(ts_month_nbp, start = c(1997,02), end = c(2022,09), frequency = 12)

# lets compare against monthly NBP and monthly TTF
plot(wb_eu_ng , ylab = "USD/MMBtu", xlab = "Time", ylim = c(1,70))
grid(nx = NULL, ny = NULL,
     lty = 2,      # Grid line type
     col = "gray") # Grid line color)      # Grid line width
par(new=TRUE)
plot(ts_graph , col = "blue", axes = FALSE, ylab = "", xlab = "",  ylim = c(1,70))
par(new=TRUE)
plot(ts_month_nbp_graph , col = "red", axes= FALSE, ylab = "", xlab = "",  ylim = c(1,70))
legend('topleft', legend=c("World Bank EU Natural Gas ", "TTF Monthly", "NBP Monthly"),
       col=c("black", "blue", "red"), lty=1, cex=1.2)


mts <- as.numeric(time(diff(log(wb_eu_ng))))
tms <- date_decimal(mts)
write.csv(data.frame(Y=as.matrix(diff(log(wb_eu_ng))), date=tms), file="wb_eu.csv")
mts <- as.numeric(time(diff(log(wb_japan_lng))))
tms <- date_decimal(mts)
write.csv(data.frame(Y=as.matrix(diff(log(wb_japan_lng))), date=tms), file="wb_jap.csv")

mts <- as.numeric(time(log(wb_eu_ng)))
tms <- date_decimal(mts)
write.csv(data.frame(Y=as.matrix(log(wb_eu_ng)), date=tms), file="wb_eu_n.csv")
mts <- as.numeric(time(log(wb_japan_lng)))
tms <- date_decimal(mts)
write.csv(data.frame(Y=as.matrix(log(wb_japan_lng)), date=tms), file="wb_jap_n.csv")


mts <- as.numeric(time(diff(diff(log(wb_eu_ng)))))
tms <- date_decimal(mts)
write.csv(data.frame(Y=as.matrix(diff(diff(log(wb_eu_ng)))), date=tms), file="wb_eu_dd.csv")
mts <- as.numeric(time(diff(diff(log(wb_japan_lng)))))
tms <- date_decimal(mts)
write.csv(data.frame(Y=as.matrix(diff(diff(log(wb_japan_lng)))), date=tms), file="wb_jap_dd.csv")


for(i in ts_names){
  ts_type = get(i)
  mts <- as.numeric(time(ts_type))
  tms <- date_decimal(mts)
  file_name <- paste(i, "csv", sep= ".")
  write.csv(data.frame(Y=as.matrix(ts_type), date=tms), file=file_name)
}

for(i in)
  









