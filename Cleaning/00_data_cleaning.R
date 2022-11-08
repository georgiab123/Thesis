# initial cleaning of exp data set
# LOADING PACKAGES ############################################################

library(dplyr)
library(lubridate)
library(ggfortify)
library(stats)

# IMPORTING DATA #############################################################

exp <- LNG_exports_repository

# CLEANING ####################################################################

term_names <- unique(exp$departure_terminal)
exp$date <- as.Date(exp$date, format = "%m/%d/%Y")

exp <- exp%>% 
    mutate(month = format(date, "%m"), year = format(date, "%Y"), year_month = format(date, "%Y-%m"))

# DATA EXPLORATION ############################################################

exp_terminals <- exp %>% 
    group_by(departure_terminal, year, month) %>%
    summarise(monthly_mean = mean(price_usd_mmbtu), monthly_mean_NA = mean(price_usd_mmbtu, na.rm = TRUE))

mean(exp$price_usd_mmbtu, na.rm = TRUE)
mean(exp_terminals$monthly_mean, na.rm=TRUE)

sabine_prices <- subset(exp_terminals,  exp_terminals$departure_terminal == "Sabine Pass, Louisiana")
christi_prices <- subset(exp_terminals,  exp_terminals$departure_terminal == "Corpus Christi, Texas")


# WEIGHTED MONTHLY PRICES ####################################################
# recreating weighed monthly prices
exp_weighted_prices <- exp %>% 
  group_by(departure_terminal, year, month) %>%
  summarise(monthly_mean = mean(price_usd_mmbtu), total_gas = sum(volume_mcf))

# testing one weighted mean that we have been given, we see the below is the correct way to calculate
# the weighted mean given by the DOE

weighted_jan_20 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "01" & exp_weighted_prices$year == "2019")
weights = c(weighted_jan_20$total_gas/sum(weighted_jan_20$total_gas))
weighted.mean(weighted_jan_20$monthly_mean, weights)

# I can automate the below but for now am doing it manually
months <- c("03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

weighted_dec_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "12" & exp_weighted_prices$year == "2018")
weighted_nov_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "11" & exp_weighted_prices$year == "2018")
weighted_oct_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "10" & exp_weighted_prices$year == "2018")
weighted_sep_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "09" & exp_weighted_prices$year == "2018")
weighted_aug_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "08" & exp_weighted_prices$year == "2018")
weighted_jul_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "07" & exp_weighted_prices$year == "2018")
weighted_jun_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "06" & exp_weighted_prices$year == "2018")
weighted_may_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "05" & exp_weighted_prices$year == "2018")
weighted_apr_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "04" & exp_weighted_prices$year == "2018")
weighted_mar_18 <- subset(exp_weighted_prices,  exp_weighted_prices$month == "03" & exp_weighted_prices$year == "2018")

weights = c(weighted_dec_18$total_gas/sum(weighted_dec_18$total_gas))
weighted.mean(weighted_dec_18$monthly_mean, weights)

weights = c(weighted_nov_18$total_gas/sum(weighted_nov_18$total_gas))
weighted.mean(weighted_nov_18$monthly_mean, weights)

weights = c(weighted_oct_18$total_gas/sum(weighted_oct_18$total_gas))
weighted.mean(weighted_oct_18$monthly_mean, weights)

weights = c(weighted_sep_18$total_gas/sum(weighted_sep_18$total_gas))
weighted.mean(weighted_sep_18$monthly_mean, weights)

weights = c(weighted_aug_18$total_gas/sum(weighted_aug_18$total_gas))
weighted.mean(weighted_aug_18$monthly_mean, weights)

weights = c(weighted_jul_18$total_gas/sum(weighted_jul_18$total_gas))
weighted.mean(weighted_jul_18$monthly_mean, weights)

weights = c(weighted_jun_18$total_gas/sum(weighted_jun_18$total_gas))
weighted.mean(weighted_jun_18$monthly_mean, weights)

weights = c(weighted_may_18$total_gas/sum(weighted_may_18$total_gas))
weighted.mean(weighted_may_18$monthly_mean, weights)

weights = c(weighted_apr_18$total_gas/sum(weighted_apr_18$total_gas))
weighted.mean(weighted_apr_18$monthly_mean, weights)

weights = c(weighted_mar_18$total_gas/sum(weighted_mar_18$total_gas))
weighted.mean(weighted_mar_18$monthly_mean, weights)

# EXAMINING SHORT/LONG COUNTS ##################################################

# examining the docket term count 
exp$docket_term[exp$docket_term =="Short-Term"] <- 1
exp$docket_term[exp$docket_term =="Long-Term"] <- 2
exp$docket_term 
exp_dockets <- exp  %>% 
    group_by(year, month) %>%
    summarise(count_short = length(which(docket_term == 1)), count_long = length(which(docket_term == 2)))



t1 <- ts(exp_dockets$count_short, start=c(2016, 2), end=c(2022, 7), frequency=12)
t2 <- ts(exp_dockets$count_long, start=c(2016, 2), end=c(2022, 7), frequency=12)

ts_dockets<- cbind(t1,t2)
colnames(ts_dockets) <- c("Short Term", "Long Term") 


# GRAPH 1
autoplot(ts_dockets, facets = FALSE) +  labs(x = "Time", y = "Monthly Exports") + labs(color="Type") 

# EXAMINING SPOT/NON-SPOT COUNTS ###############################################
#Examining how frequency of spot or long term has changed:
notes_tags <- unique(exp$notes)


interest <- c(1,4,5,6,9,10,13,15,16,22,25,26,27)
j = 1
all_numbers <- c()
no_interest <- c()
for (i in notes_tags){
    exp$notes[exp$notes ==i] <- j
    all_numbers <- append(all_numbers, j)
    if (! j %in% interest){
        no_interest <- append(no_interest, j)
    }
    j = j + 1 
}


exp_spot <- exp  %>% 
    group_by(year, month) %>%
    summarise(spot_contracts = length(which(notes %in% interest)), all_contracts =  length(which(notes %in% all_numbers)), non_spot_contracts =  length(which(notes %in% no_interest)))

t3 <- ts(exp_spot$spot_contracts, start=c(2016, 2), end=c(2022, 7), frequency=12)
t4 <- ts(exp_spot$all_contracts, start=c(2016, 2), end=c(2022, 7), frequency=12)
t5 <- ts(exp_spot$non_spot_contracts, start=c(2016, 2), end=c(2022, 7), frequency=12)
contracts_ts <- cbind(t3,t4, t5)
colnames(contracts_ts) <- c("Spot types", "Any type", "Non-spot")

# GRAPH 2
autoplot(contracts_ts, facets = FALSE) +  labs(x = "Time", y = "Spot denoted contracts") + labs(color="Type") 

# GRAPH 3

#plotting these five time series together:
five_ts <- cbind(contracts_ts, ts_dockets)
colnames(five_ts) <- c("Spot contracts", "All contract types", "Non-spot contracts", "Short-term contracts", "Long-term contracts")
autoplot(five_ts, facets = FALSE) +  labs(x = "Time", y = "Contract types of daily exports, aggregated by month") + labs(color="Type") 


# GROUPING BY UNIQUE DOCKET NUMBER ############################################

# each unique docket number represents a contract type
# there are 40 docket numbers in this data set
docket_numbers <- unique(exp$docket_number)

data_base <- c()
for(i in docket_numbers){
  name <- paste("num", i, sep = "_")
  sub_docket <- subset(exp, exp$docket_number == i)
  assign(name, sub_docket)
}

# we can investigate a few of these contracts:
# examining num_2011_162-LNG
unique(`num_2011-162-LNG`$price_usd_mmbtu)
# it only has NA values
# lets check num_2012-97-LNG
plot(`num_2012-97-LNG`$date, `num_2012-97-LNG`$price_usd_mmbtu)

# ITS VERY HARD to plot by contract ID


#exp_contracts <- exp %>% 
#  group_by(docket_number, month, year) %>%
#  summarise(monthly_mean = mean(price_usd_mmbtu), total_gas = sum(volume_mcf))



# GROUPING BY L (liquefaction fee) OR NOT #####################################










