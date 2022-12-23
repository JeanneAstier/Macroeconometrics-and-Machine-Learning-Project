
### package for specific models ###
# library(devtools)
# install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)
library(glmnet)
library(randomForest)

# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("functions/rolling_window.R")
source("functions/functions.R")

#####
## The file with the forecasts will be saved with model_name
model_name = "CSR"
## The function called to run models is model_function, which is a function from functions.R
model_function = runcsr
#####


load("data/data.rda")
dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

####### run rolling window ##########
nwindows = 312
model_list = list()

for(i in 1:12){
  old <- Sys.time() # get start time
  model = rolling_window(model_function,data,nwindows+i-1,i,"CPIAUCSL")
  model_list[[i]] = model
  cat(i,"\n")
  new <- Sys.time() - old # calculate difference
  print(new) # print in nice format
}


forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))

forecasts = accumulate_model(forecasts)

save(forecasts,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],312),type = "l")
lines(forecasts[,1],col = 3)