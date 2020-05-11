#############################################
# This file reads in the data and stores in R dataframe
#############################################
rm(list = ls())
library(reader)

file_names = c(paste(month.name, "2018.csv", sep = ""), paste(month.name, "2019.csv", sep = ""))
file_path = "/Users/rachaelshudde/Desktop/Data/AirlineData"

# put each month in a matrix in a list called "data"
data = list()
count = 0
for (i in file_names)
{
  temp_file = find.file(i, dir = file_path, dirs = NULL)

  data[[i]] = read.csv(temp_file)
  show(paste("read in:", temp_file))
  
  count = count + nrow(data[[i]]) # number of total rows
  
}

# combine all the data
data_combined = do.call(rbind, data)

selected_columns = c("Year", "Quarter", "Month", "DayofMonth", "DayOfWeek", "FlightDate", "Marketing_Airline_Network",
             "Tail_Number", "Flight_Number_Operating_Airline", "Origin", "Dest", "CRSDepTime", "DepTime", "DepDelay", 
             "CRSArrTime", "ArrTime", "ArrDelay", "Cancelled", "Diverted", "AirTime", "Distance", "TaxiOut", "TaxiIn",
             "WheelsOff", "WheelsOn")

data_final = data_combined[, selected_columns]
rownames(data_final) = 1:nrow(data_final)

save(data_final,file = "airline_data.Rda")

# load("airline_data.Rda")
