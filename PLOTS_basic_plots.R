#############################################
# This file ris some basic plots of the data
# just in base package, can switch to ggplot later
#############################################
library(dplyr)

load("airline_data.Rda")

## 
yearone = filter(data_final, Year == 2018)
delayed = filter(yearone, DepDelay > 5)


total = table(yearone$Month)
partial = table(delayed$Month)
prop.test(as.vector(partial), as.vector(total)) # conclude some months are more significant


total = table(yearone$DayofMonth)
partial = table(delayed$DayofMonth)
prop.test(as.vector(partial), as.vector(total)) # conclude some days of month are more significant


total = table(yearone$DayOfWeek)
partial = table(delayed$DayOfWeek)
prop.test(as.vector(partial), as.vector(total)) # conclude some days of week are more significant


total = table(yearone$Quarter)
partial = table(delayed$Quarter)
prop.test(as.vector(partial), as.vector(total)) # conclude some quarters of week are more significant



############################################################## best predictors
library(xts)
library(chron)

yearone = yearone[complete.cases(yearone), ]
yearone$date = as.Date(yearone$FlightDate,"%Y-%m-%d")
temp2 <- mapply(function(x, y) paste0(rep(x, y), collapse = ""), 0, 4 - nchar(yearone$CRSDepTime))
temp <- paste0(temp2, yearone$CRSDepTime)

yearone$time = format(strptime(temp, format="%H%M"), format = "%H:%M")

yearone$dateandtime = as.POSIXct(paste(yearone$date, yearone$time), format="%Y-%m-%d %H:%M", tz = "America/Chicago")

unique = yearone %>% group_by(yearone$dateandtime) %>% summarize(time_mean = mean(DepDelay))
colnames(unique) = c("Date", "Delay")
unique = unique[-418285, ]

delays <- xts(unique$Delay, unique$Date)


plot.ts(delays[1:500])
stl(delays, s.window="periodic", robust = T) 




xts::plot.xts(as.xts(delays), main = "Total")


########################################################## tail numbers
par(mfrow = c(3,4))
for (i in unique(yearone$Marketing_Airline_Network))
{
  print(i)

  yearone_airline = yearone[yearone$Marketing_Airline_Network == i, ]
  yearone_airline$date = as.Date(yearone_airline$FlightDate,"%Y-%m-%d")

  unique = yearone_airline %>% group_by(yearone_airline$date) %>% summarize(time_mean = mean(DepDelay))
  colnames(unique) = c("Date", "Delay")
  unique = unique[complete.cases(unique), ]
  
  delays <- xts(unique$Delay, unique$Date)
  
  show(xts::plot.xts(as.xts(delays), main = i))
}


yeartwo = filter(data_final, Year == 2019)
yeartwo = yeartwo[complete.cases(yeartwo), ]


par(mfrow = c(3,4))
for (i in unique(yeartwo$Marketing_Airline_Network))
{
  print(i)
  
  yeartwo_airline = yeartwo[yeartwo$Marketing_Airline_Network == i, ]
  yeartwo_airline$date = as.Date(yeartwo_airline$FlightDate,"%Y-%m-%d")

  unique = yeartwo_airline %>% group_by(yeartwo_airline$date) %>% summarize(time_mean = mean(DepDelay))
  colnames(unique) = c("Date", "Delay")
  unique = unique[complete.cases(unique), ]
  
  delays <- xts(unique$Delay, unique$Date)
  
  show(xts::plot.xts(as.xts(delays), main = i))
}


for (i in unique(yeartwo$Marketing_Airline_Network))
{
  print(i)
  
  ## year 1
  yearone_airline = yearone[yearone$Marketing_Airline_Network == i, ]
  yearone_airline$date = as.Date(yearone_airline$FlightDate,"%Y-%m-%d")
  
  unique = yearone_airline %>% group_by(yearone_airline$date) %>% summarize(time_mean = mean(DepDelay))
  colnames(unique) = c("Date", "Delay")
  unique = unique[complete.cases(unique), ]
  
  delays <- xts(unique$Delay, unique$Date)
  
  ## year 2
  yeartwo_airline = yeartwo[yeartwo$Marketing_Airline_Network == i, ]
  yeartwo_airline$date = as.Date(yeartwo_airline$FlightDate,"%Y-%m-%d")
  
  unique = yeartwo_airline %>% group_by(yeartwo_airline$date) %>% summarize(time_mean = mean(DepDelay))
  colnames(unique) = c("Date", "Delay")
  unique = unique[complete.cases(unique), ]
  
  delays2 <- xts(unique$Delay, unique$Date)
  

  par(mfrow = c(1,2))
  show(xts::plot.xts(as.xts(delays), main = i))
  show(xts::plot.xts(as.xts(delays2), main = i))
}

############################################################ linear regression 
library(lme4)
y = yearone$DepDelay
x = yearone[, -14]

model1 = lm(DepDelay ~ as.factor(Quarter) + as.factor(Month) + as.factor(DayofMonth) + as.factor(DayOfWeek) +
                as.factor(Marketing_Airline_Network) + Tail_Number + as.factor(Origin) +
                as.factor(Dest), data = yearone[sample(1:nrow(yearone), 50000, replace = FALSE), ])




for (i in unique(yeartwo$Marketing_Airline_Network))
{
  a = (length(which(data_final$Marketing_Airline_Network == i)) / nrow(data_final) * 100)
  show(paste("Air carrier", i, "has market share", round(a, 1)))
}




