# LOADING PACKAGES ############################################################

library(dplyr)
library(lubridate)
library(ggfortify)
library(stats)
library(forecast)
library(tibbletime)
library(xlsx)

# IMPORTING DATA ###############################################################

jkm_data <- JKM
nbp_data <- NBP

# CLEANING ####################################################################

# we can now convert these into time series


jkm_data <- jkm_data[-1,] 
jkm_data$year_month <- floor_date(jkm_data$Date, "month")
jkm_data$JKMc1 <- as.numeric(jkm_data$JKMc1)


nbp_data <- nbp_data[-1,] 
nbp_data$Date <- as.Date(nbp_data$Date)
nbp_data$year_month <- floor_date(nbp_data$Date, "month")
nbp_data$NGLNMc1 <- as.numeric(nbp_data$NGLNMc1)

jkm_data_monthly <- jkm_data %>%                   
  group_by(year_month) %>% 
  summarise(monthly_price = mean(JKMc1))

nbp_data_monthly <- nbp_data %>%                   
  group_by(year_month) %>% 
  summarise(monthly_price = mean(NGLNMc1))


# EXPORTING ##################################################################

#exported to to Thesis_gb
write.xlsx(jkm_data_monthly, file = "jkm_data_monthly.xlsx") 
write.xlsx(nbp_data_monthly, file = "nbp_data_monthly.xlsx") 
write.csv2(jkm_data_monthly, file = "jkm_data_monthly.csv")
write.csv2(nbp_data_monthly, file = "nbp_data_monthly.csv")


# first date of JKM is 2012-09-26 up to 2022-11-02
# first date of NBP is 1997-01-30 up to 2022-11-02



#16th row and 9th column


