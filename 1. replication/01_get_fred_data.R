
# library dedicated to handle FRED data
library(devtools)
install_github("cykbennie/fbi", force = TRUE) 

# other libraries
library(fbi)
library(tidyverse)
library(TTR)


# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

start_date = "1960-01-01"

# 1. loading data from FRED-MD

# the 'fredmd' function by defaults transform the data to stationnarize it (first difference, second difference, log, first or second difference of log, first difference of percent point).
data = fredmd("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2016-02.csv")

# download of data without transformation
data_raw = fredmd("https://files.stlouisfed.org/files/htdocs/fred-md/monthly/2016-02.csv", transform = FALSE)

# meta-data about the FRED variables
varlist = fredmd_description
vars = intersect(colnames(data),varlist$fred)

# 2. data cleaning

# keep only variables for which we have meta-data
data = data %>% as_tibble()%>%
  select(all_of(c("date",vars)))
varlist = varlist%>%filter(fred%in%vars)

# additional transformation for price variables : simple differenciation of logs (= inflation)
prices_varlist = varlist%>%filter(group=="Prices",tcode==6)
data = data%>% as_tibble()%>%
  select( -all_of(prices_varlist$fred) )
prices = data_raw %>% as_tibble() %>%
  select(all_of(prices_varlist$fred))%>%
  mutate_all(.funs = function(x)100*c(NA,x%>%log()%>%diff()))
data = cbind(data%>%as.data.frame(),prices%>%as.data.frame())

# we keep only variables with all observations in the sample period 
data = data %>%
  filter(date>=start_date)%>%
  select_if(~ !any(is.na(.)))

save(data,file = "data/data.rda")

