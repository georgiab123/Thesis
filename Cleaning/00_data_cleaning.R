# initial cleaning of exp data set
# LOADING PACKAGES ############################################################

library(dply)
library(lubridate)
library(ggfortify)

# IMPORTING DATA ############################################################
exp <- LNG_exports_repository
term_names <- unique(exp$departure_terminal)


exp$date <- as.Date(exp$date, format = "%m/%d/%Y")

exp <- exp%>% 
    mutate(month = format(date, "%m"), year = format(date, "%Y"), year_month = format(date, "%Y-%m"))



exp_terminals <- exp %>% 
    group_by(departure_terminal, year, month) %>%
    summarise(monthly_mean = mean(price_usd_mmbtu), monthly_mean_NA = mean(price_usd_mmbtu, na.rm = TRUE))


mean(exp$price_usd_mmbtu, na.rm = TRUE)
mean(exp_terminals$monthly_mean, na.rm=TRUE)

sabine_prices <- subset(exp_terminals,  exp_terminals$departure_terminal == "Sabine Pass, Louisiana")
sabine_prices


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


# more aesthetic plots for the dockets
library(ggfortify)
autoplot(ts_dockets, facets = FALSE) +  labs(x = "Time", y = "Monthly Exports") + labs(color="Type") 


#Examining how frequency of spot or long term has changed:
notes_tags <- unique(exp$notes)

#
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

interest <- c(1,4,5,6,9,10,13,15,16,22,25,26,27)

exp_spot <- exp  %>% 
    group_by(year, month) %>%
    summarise(spot_contracts = length(which(notes %in% interest)), all_contracts =  length(which(notes %in% all_numbers)), non_spot_contracts =  length(which(notes %in% no_interest)))

t3 <- ts(exp_spot$spot_contracts, start=c(2016, 2), end=c(2022, 7), frequency=12)
t4 <- ts(exp_spot$all_contracts, start=c(2016, 2), end=c(2022, 7), frequency=12)
t5 <- ts(exp_spot$non_spot_contracts, start=c(2016, 2), end=c(2022, 7), frequency=12)
contracts_ts <- cbind(t3,t4, t5)
colnames(contracts_ts) <- c("Spot types", "Any type", "Non-spot")
autoplot(contracts_ts, facets = FALSE) +  labs(x = "Time", y = "Spot denoted contracts") + labs(color="Type") 


#plotting these four time series together:
five_ts <- cbind(contracts_ts, ts_dockets)
colnames(five_ts) <- c("Spot contracts", "All contract types", "Non-spot contracts", "Short-term contracts", "Long-term contracts")
autoplot(five_ts, facets = FALSE) +  labs(x = "Time", y = "Contracts") + labs(color="Type") 




