# initial cleaning of exp data set
# LOADING PACKAGES ############################################################

library(dplyr)
library(lubridate)
library(ggfortify)
library(stats)

# IMPORTING DATA #############################################################

exp <- LNG_exports_repository

# DATA MANIPULATION ############################################################

#term_names <- unique(exp$departure_terminal)
exp$date <- as.Date(exp$date, format = "%m/%d/%Y")

exp <- exp%>% 
  mutate(month = format(date, "%m"), year = format(date, "%Y"), year_month = format(date, "%Y-%m"))

exp_dest <- exp %>% 
  group_by(country_of_destination,year_month) %>%
  summarise(vol_num = sum(volume_mcf)) 

# first let us group these countries

country_names <- unique(exp$country_of_destination)

S.America <- c("Brazil", "Argentina", "Kuwait", "Chile", "Dominican Republic", "Panama", "Colombia", "Jamaica")

Asia <- c("India", "United Arab Emirates", "Kuwait", "Jordan", "China", "Turkey", "South Korea", "Japan",
         "Pakistan", "Thailand", "Taiwan", "Israel", "Singapore", "Malaysia", "Bangladesh", "Indonesia")

Africa <- c("Egypt")

Europe <- c("Portugal", "Spain", "Italy", "Malta", "Netherlands", "Poland", "United Kingdom", "Lithuania",
            "France", "Greece", "Belgium", "Croatia")

ME_NA <- c("Mexico")


index_SAMERICA <- exp_dest$country_of_destination %in% S.America
index_ASIA <- exp_dest$country_of_destination %in% Asia
index_AFRICA <- exp_dest$country_of_destination %in% Africa
index_EUROPE <- exp_dest$country_of_destination %in% Europe
index_ME_NA <- exp_dest$country_of_destination %in% ME_NA

exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_SAMERICA , 1)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_ASIA , 2)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_AFRICA , 3)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_EUROPE , 4)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_ME_NA , 5)

# now lets create subsets for each of these regions: 

exp_dest_samerica <- subset(exp_dest, exp_dest$country_of_destination == 1)
exp_dest_asia <- subset(exp_dest, exp_dest$country_of_destination == 2)
exp_dest_africa <- subset(exp_dest, exp_dest$country_of_destination == 3)
exp_dest_eu <- subset(exp_dest, exp_dest$country_of_destination == 4)
exp_dest_me_na <- subset(exp_dest, exp_dest$country_of_destination == 5)

exp_dest_samerica <- exp_dest_samerica %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_asia <- exp_dest_asia %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_africa  <- exp_dest_africa  %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_eu <- exp_dest_eu %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_me_na <- exp_dest_me_na %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

vec_names <- c("exp_dest_samerica", "exp_dest_asia", "exp_dest_africa",
               "exp_dest_eu", "exp_dest_me_na")

for(i in vec_names){
raw.data <- get(i) 
name <- paste("d", i, sep = "")
raw.data$time <- raw.data$year_month
raw.data$time <- anydate(raw.data$time)
raw.data$time <- as.Date(raw.data$time)

sorted.data <- raw.data
data.length <- length(sorted.data$time)
time.min <- sorted.data$time[1]
time.max <- sorted.data$time[data.length]
all.dates <- seq(time.min, time.max, by="month")
all.dates.frame <- data.frame(list(time=all.dates))

merged.data <- merge(all.dates.frame, sorted.data, all=T)
merged.data <- merged.data[,-2]
merged.data$vol_num_total[which(is.na(merged.data$vol_num_total))] <- 0
assign(name, merged.data)
}

# divide all the values by 100 to get to quite in billions
#only run the below ONCE

dexp_dest_samerica$vol_num_total <- dexp_dest_samerica$vol_num_total/1000
dexp_dest_asia$vol_num_total <- dexp_dest_asia$vol_num_total/1000
dexp_dest_africa$vol_num_total <- dexp_dest_africa$vol_num_total/1000
dexp_dest_eu$vol_num_total <- dexp_dest_eu$vol_num_total/1000
dexp_dest_me_na$vol_num_total <- dexp_dest_me_na$vol_num_total/1000

term_ts_1 <- ts(dexp_dest_samerica$vol_num_total, start=c(2016, 2), end=c(2022, 7), frequency=12)
term_ts_2 <- ts(dexp_dest_asia$vol_num_total, start=c(2016, 3), end=c(2022, 7), frequency=12)
term_ts_3 <- ts(dexp_dest_africa$vol_num_total , start=c(2016, 12), end=c(2018, 01), frequency=12)
term_ts_4 <- ts(dexp_dest_eu$vol_num_total, start=c(2016, 4), end=c(2022, 7), frequency=12)
term_ts_5 <- ts(dexp_dest_me_na$vol_num_total, start=c(2016, 8), end=c(2022, 6), frequency=12)



term_ts_p <- cbind(term_ts_1,term_ts_2,term_ts_3,term_ts_4,term_ts_5)

colnames(term_ts_p) <- c("South America", "Asia", "Africa", "Europe", "Mexico")

autoplot(term_ts_p, facets = FALSE) + labs(x = "Time", y = "Volume (Bcf)") + 
  labs(color="Destination") + theme_bw() + scale_y_continuous(labels=scales::comma) +
  theme(axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))


# DF: EUROPE SPOTS ############################################################

# we can split contracts into S or not S and then group by region of destination: 

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

exp_spot_dest <- exp  %>% 
  group_by(year_month, country_of_destination) %>%
  summarise(spot_contracts = length(which(notes %in% interest)),
            all_contracts =  length(which(notes %in% all_numbers)),
            non_spot_contracts =  length(which(notes %in% no_interest)))

index_SAMERICA_s <- exp_spot_dest$country_of_destination %in% S.America
index_ASIA_s <- exp_spot_dest$country_of_destination %in% Asia
index_AFRICA_s <- exp_spot_dest$country_of_destination %in% Africa
index_EUROPE_s <- exp_spot_dest$country_of_destination %in% Europe
index_ME_NA_s <- exp_spot_dest$country_of_destination %in% ME_NA

exp_spot_dest$country_of_destination <- replace(exp_spot_dest$country_of_destination, index_SAMERICA_s , 1)
exp_spot_dest$country_of_destination <- replace(exp_spot_dest$country_of_destination, index_ASIA_s , 2)
exp_spot_dest$country_of_destination <- replace(exp_spot_dest$country_of_destination, index_AFRICA_s , 3)
exp_spot_dest$country_of_destination <- replace(exp_spot_dest$country_of_destination, index_EUROPE_s , 4)
exp_spot_dest$country_of_destination <- replace(exp_spot_dest$country_of_destination, index_ME_NA_s , 5)

# now lets create subsets for each of these regions: 

exp_dest_samerica_s <- subset(exp_spot_dest, exp_spot_dest$country_of_destination == 1)
exp_dest_asia_s <- subset(exp_spot_dest, exp_spot_dest$country_of_destination == 2)
exp_dest_africa_s <- subset(exp_spot_dest, exp_spot_dest$country_of_destination == 3)
exp_dest_eu_s <- subset(exp_spot_dest, exp_spot_dest$country_of_destination == 4)
exp_dest_me_na_s <- subset(exp_spot_dest, exp_spot_dest$country_of_destination == 5)

# now lets just examine Europe


raw.data <- exp_dest_eu_s 
raw.data$time <- raw.data$year_month
raw.data$time <- anydate(raw.data$time)
raw.data$time <- as.Date(raw.data$time)

sorted.data <- raw.data
data.length <- length(sorted.data$time)
time.min <- sorted.data$time[1]
time.max <- sorted.data$time[data.length]
all.dates <- seq(time.min, time.max, by="month")
all.dates.frame <- data.frame(list(time=all.dates))

merged.data <- merge(all.dates.frame, sorted.data, all=T)
merged.data <- merged.data[,-2]

#merged.data <- merged.data[,-2]
merged.data$vol_num_total[which(is.na(merged.data$spot_contractsl))] <- 0

# now lets create three time series for this: 

term_ts_1 <- ts(merged.data$spot_contracts, start=c(2016, 4), end=c(2022, 7), frequency=12)
term_ts_2 <- ts(merged.data$all_contracts, start=c(2016, 4), end=c(2022, 7), frequency=12)
term_ts_3 <- ts(merged.data$non_spot_contracts , start=c(2016, 4), end=c(2022, 7), frequency=12)

term_ts_p <- cbind(term_ts_1,term_ts_2,term_ts_3)
colnames(term_ts_p) <- c("Spot contracts", "All contracts", "Non spot contracts")
autoplot(term_ts_p, facets = FALSE) + labs(x = "Time", y = "# contracts") + labs(color="Type")


# the above graph is not very useful
# instead let us group by dockets and look how their destination is changng

# there are 40 unique docket_numbers
# we can group by docket number ann look at how region of destination and graph region 
# of destination for each of them:
docket_names <- unique(exp_dest$docket_number)
for(i in docket_names){
  name <- paste("dnum", i, sep = "_")
  i <- subset(exp, exp$docket_number == i)
  assign(name, i)
}



exp_dest <- `dnum_16-205-LNG`


exp_dest <- exp_dest %>% 
  group_by(country_of_destination,year_month) %>%
  summarise(vol_num = sum(volume_mcf)) 

country_names <- unique(exp$country_of_destination)



index_SAMERICA <- exp_dest$country_of_destination %in% S.America
index_ASIA <- exp_dest$country_of_destination %in% Asia
index_AFRICA <- exp_dest$country_of_destination %in% Africa
index_EUROPE <- exp_dest$country_of_destination %in% Europe
index_ME_NA <- exp_dest$country_of_destination %in% ME_NA

exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_SAMERICA , 1)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_ASIA , 2)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_AFRICA , 3)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_EUROPE , 4)
exp_dest$country_of_destination <- replace(exp_dest$country_of_destination, index_ME_NA , 5)

# now lets create subsets for each of these regions: 

exp_dest_samerica <- subset(exp_dest, exp_dest$country_of_destination == 1)
exp_dest_asia <- subset(exp_dest, exp_dest$country_of_destination == 2)
exp_dest_africa <- subset(exp_dest, exp_dest$country_of_destination == 3)
exp_dest_eu <- subset(exp_dest, exp_dest$country_of_destination == 4)
exp_dest_me_na <- subset(exp_dest, exp_dest$country_of_destination == 5)

exp_dest_samerica <- exp_dest_samerica %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_asia <- exp_dest_asia %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_africa  <- exp_dest_africa  %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_eu <- exp_dest_eu %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

exp_dest_me_na <- exp_dest_me_na %>% 
  group_by(year_month) %>%
  summarise(vol_num_total = sum(vol_num)) 

vec_names <- c("exp_dest_samerica", "exp_dest_asia", "exp_dest_africa",
               "exp_dest_eu", "exp_dest_me_na")


vec_names_REAL <- c()
for(i in vec_names){
  raw.data <- get(i) 

  raw.data$time <- raw.data$year_month
  raw.data$time <- anydate(raw.data$time)
  raw.data$time <- as.Date(raw.data$time)
  
  sorted.data <- raw.data
  data.length <- length(sorted.data$time)
  time.min <- sorted.data$time[1]
  time.max <- sorted.data$time[data.length]

  if(is.na(time.min) == TRUE){
    next
  }
  
  name <- paste("d", i, sep = "")
  vec_names_REAL <- append(vec_names_REAL, name)
  all.dates <- seq(time.min, time.max, by="month")
  all.dates.frame <- data.frame(list(time=all.dates))
  
  merged.data <- merge(all.dates.frame, sorted.data, all=T)
  merged.data <- merged.data[,-2]
  merged.data$vol_num_total[which(is.na(merged.data$vol_num_total))] <- 0
  assign(name, merged.data)
}

vec_names_REAL <- unique(vec_names_REAL)

j = 1
ts_names <- c()
for(i in vec_names_REAL){
  name <- paste("ts", j, sep = "_")
  j = j + 1
  ts_names <- append(ts_names, name)

  dates <- as.list(str_split(get(i)[1,1], "-")[[1]])
  year_min <- unlist(dates[1])
  month_min <- unlist(dates[2])
  
  max_dim <- dim(get(i))[1]
  dates_max <- as.list(str_split(get(i)[max_dim,1], "-")[[1]])
  year_max <- unlist(dates_max[1])
  month_max <- unlist(dates_max[2])
  
  assign(name, ts(get(i)$vol_num_total, start=c(year_min, month_min),
                  end=c(year_max, month_max), frequency=12))
}

ts_data <- c()
for(i in ts_names){
  if(length(ts_names)==1){
    ts_data <- get(ts_names[1])
    break 
  }
  ts_data <- cbind(ts_data, get(i)) 
}

colnames(ts_data) <- vec_names_REAL
plot <- autoplot(ts_data, facets = FALSE) + labs(x = "Time", y = "Volume (BCF)") + labs(color="Type")





