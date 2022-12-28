library(fbi)
library(tidyverse)
library(TTR)
library("readxl")
library(lubridate)

# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

start_date = "1985-01-01"
end_date = "2020-12-01"

# 1. loading data from excel (downloaded from FRED monthly data for France)

data <- read_excel("data/data_France.xls", sheet = 2)
data$DATE <- ymd(data$DATE)
data <- data.frame(data)


# we keep only variables with all observations in the sample period 
data = data[data$DATE >= start_date, ]
data = data[data$DATE <= end_date, ]
data = data[ , apply(data, 2, function(x) !any(is.na(x)))]

# total : 86 variables
# 1985 - 2020 : 63 variables

# 2. data cleaning

# stationnarization of the data: simple differences (except for inflation : already differenciated)
data_transform = data[, names(data) != "CPI"]

for (i1 in 2:length(data_transform))
{
  diffy <- diff(as.numeric(data_transform[,i1]))
  data_transform[,i1] <- c(diffy, NA)
}

# add inflation data
inflation = data[, names(data) %in% c("DATE", "CPI")] 
inflation$CPI <- as.numeric(inflation$CPI)
inflation$CPI <- inflation$CPI/100

data = merge(x = data_transform, y = inflation, by = "DATE", all = TRUE)

names(data)[names(data) == "DATE"] <- "date"
names(data)

# set dates as index
# data_new <- data[,-1]
# rownames(data_new) <- data[,1]
# data = data_new

save(data,file = "data/data.rda")

