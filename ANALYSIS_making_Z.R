rm(list = ls())
library(dplyr)

load("airline_data.Rda")
# data = data_final[1:500000, ]
data = data_final
data = data[complete.cases(data), ]

# set up Y variables
columns = c("Flight_Number_Operating_Airline", "DepDelay", "FlightDate")
Y = data[, columns]
colnames(Y) = c("num", "delay", "date")
Y = Y[order(as.Date(Y$date, format="%Y-%m-%d")),]
Y$date = as.Date(Y$date, format="%Y-%m-%d")

Y = Y %>% group_by(num, date) %>% summarize(delay = mean(delay, na.rm = TRUE))
Y = data.frame(Y)
head(Y)

# get minimum and miximum for range
min = min(Y$date)
max = max(Y$date)
range = length(numeric(max-min)) + 1

# set up range of dates
dates_list = format( seq(c(ISOdate(2018,1,1)), by = "DSTday", length.out = range),"%Y-%m-%d")
flights = unique(Y$num)
date_range = data.frame(dates_list)
colnames(date_range) = "date"
date_range$date = as.Date(date_range$date, format="%Y-%m-%d")

# initialize the overall dataframe
Z = matrix(NA, nrow = length(unique(Y$num)), ncol = length(dates_list))
colnames(Z) = dates_list

# get the Z
count = 1
for (i in unique(Y$num))
{
  #print(paste("Attempt?ing to figure out for: ", i))
  temp = Y[which(Y$num == i), ]
  a = merge(temp, date_range, by = "date", all.y = TRUE)
  Z[count, ] = a$delay
  count = count + 1
}

print(dim(Z))
percent_missing = apply(Z, 1, function(row) sum(is.na(row)) / length(row)*100)
hist(percent_missing)