
#### gets out of sample y and computes random walk forecasts ###

# libraries
library(roll) # library for rolling window framework

# set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load data
load("data/data.rda")
dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

# out-of-sample window to compare performances of models

# subsample period : Jan2005 to Dec2020 (180 obs)
nwindows = 180


# 1. out of sample true inflation (y)

# y = inflation computed from baseline price index (CPI)
y = data[,"CPI"]

# for forecasts for the accumulated inflation over the following 3, 6, and 12 months 
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1) # rolling products

yout = tail(y,nwindows) # true value of inflation, to compare with forecasts

# 2. benchmark model: random walk forecasts

# empty matrix
rw = matrix(NA,nwindows,12)

# forecast at date t for inflation(t+h) is inflation(t)
for(i in 1:12){
  aux = data[(nrow(data)-nwindows-i+1):(nrow(data)-i),"CPI"]
  rw[,i]=aux;
}

# forecast at date t for accumulated h-month inflation(t+1:t+h) is inflation(t-(h-1):t)
rw3 = tail(embed(y[,2],4)[,4],nwindows)
rw6 = tail(embed(y[,3],7)[,7],nwindows)
rw12 = tail(embed(y[,4],13)[,13],nwindows)
rw = cbind(rw,rw3,rw6,rw12)
colnames(rw) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")


save(yout,file = "forecasts/yout.rda")
save(rw,file = "forecasts/rw.rda")



