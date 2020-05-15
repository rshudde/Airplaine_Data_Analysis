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
stl(delays, s.window="periodic",robust = T) 




xts::plot.xts(as.xts(delays[]))


########################################################## tail numbers














