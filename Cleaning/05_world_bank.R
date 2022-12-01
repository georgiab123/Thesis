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


