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

data_combined = matrix(nrow = count, ncol = ncol(data[[1]])) # matrix to hold all of the new data

# put all the data into one matrix 
start = 1
for (i in 1:length(data))
{
  end = start + nrow(data[[i]]) - 1
  data_combined[start:end, ] = data[[i]]
  
  start = end + 1
}

