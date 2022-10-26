# Loading packages ############################################################

library(dplyr)
library(lubridate)
library(ggplot2)

# Importing data ##############################################################

# from local respository
# data sources are from EIA and barchart website
HenryHub <- Excel_sheet_HH
Dutch_TTF <-  TTF_MMBTU


# Cleaning data ###############################################################

colnames(HenryHub) <- c("Date", "Price")
colnames(Dutch_TTF) <- c("Date", "Symbol", "Open", "High", "Low", "Close")


#Dutch TTF ranges from 2017-05-22 --> 2022-09-26
#Henry Hub ranges to 2022-09-16
#Get subset of Henry Hub price data starting from 2017-05-22
HenryHub.2017.05.19 <- HenryHub %>%  
  filter(Date >= as.Date('2017-05-19') & Date <= as.Date('2022-09-16'))



# Plotting data ###############################################################

plot(Dutch_TTF$Date,                             
     Dutch_TTF$Close,
     type = "l",
     xlab = "Year",
     ylab = "Values",
    col = 2)
lines(HenryHub.2017.05.19$Date,                            
      HenryHub.2017.05.19$Price,
      type = "l",
      col = 3)
legend("topleft",                           
       c("Dutch", "USA"),
       lty = 1,
       col = 2:3)



ggplot() +
  geom_line(data = Dutch_TTF, aes(x = Date, y = Close, color="Dutch LNG futures"), color = "red")  +
  geom_line(data = HenryHub.2017.05.19, aes(x = Date, y = Price, color="Henry Hub spot price"), color = "blue") +
  labs(colour="Legend",x="Date",y="Dollars per one Million British Thermal Units (MMBTU)") +
  geom_vline(xintercept=as.numeric(HenryHub.2017.05.19$Date[198]), linetype=3, color = "black")


ccf(Dutch_TTF$Close,HenryHub.2017.05.19$Price)
print(ccf(Dutch_TTF$Close,HenryHub.2017.05.19$Price))
