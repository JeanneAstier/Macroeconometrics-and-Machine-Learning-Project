library(tidyverse)
library("writexl")
library(MCS) # model confidence sets procedure
library(mlRFinance) # must be installed from https://github.com/PedroBSB/mlRFinance

load("forecasts/yout.rda")
load("forecasts/rw.rda")

# load all forecasts
model_files = setdiff(list.files("forecasts/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts/",model_files[i],sep = ""))
  
  # correct accumulated predictions
  
  forecasts = forecasts[,1:(ncol(forecasts)-3)] # remove accumulated forecasts
  
  acc3 = sapply(1:(nrow(forecasts)), function(x){
    prod(1+(forecasts[x,1:3]))-1
  })
  acc6 = sapply(1:(nrow(forecasts)), function(x){
    prod(1+(forecasts[x,1:6]))-1
  })
  acc12 = sapply(1:(nrow(forecasts)), function(x){
    prod(1+(forecasts[x,1:12]))-1
  })
  
  forecasts = cbind(forecasts,acc3,acc6,acc12)
  colnames(forecasts) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")
  
  models_list[[i]] = forecasts
  
}



names(models_list) = model_files


### ROOT MEAN SQUARED ERRORS ### 

# RMSE of random walk predictions
rwe = sqrt(colMeans((rw[,1:12]-yout[,1])^2))

# MSE of other model predictions
errors = lapply(models_list, function(x){
  sqrt(colMeans((x[,1:12]-yout[,1])^2))
})%>% Reduce(f=cbind)
colnames(errors) = model_files

# MSE of accumulated predictions
rweacc = sqrt(colMeans((rw[,13:15]-yout[,2:4])^2))
errorsacc = lapply(models_list, function(x){
  sqrt(colMeans((x[,13:15]-yout[,2:4])^2,na.rm=TRUE))
})%>% Reduce(f=cbind)
colnames(errorsacc) = model_files


# final table with MSE of all models
res = rbind(errors, errorsacc)
resrw = rbind(cbind(rwe), cbind(rweacc))
res = cbind(res, resrw)
res_df = data.frame(res)

# average MSE
mean <- summarize_all(res_df, mean)
res_df <- rbind(res_df, mean)

# normalize w.r.t MSE random wals
res_mse_df <- res_df[,1:(ncol(res_df))]/res_df[, ncol(res_df)]

# max MSE
max <- summarize_all(res_mse_df, max)
res_mse_df <- rbind(res_mse_df, max)

# min MSE
min <- summarize_all(res_mse_df, min)
res_mse_df <- rbind(res_mse_df, min)

rownames(res_mse_df) <- c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12", "average", "max", "min")

# model with lower MSE for each horizon
res_mse_df$is_min <- names(res_mse_df)[apply(res_mse_df, MARGIN = 1, FUN = which.min)]


# export
write_xlsx(res_mse_df,"MSE.xlsx")






### MEAN ABSOLUTE ERROR ### 

# MEA of random walk predictions
rwe = (colMeans(abs(rw[,1:12]-yout[,1])))

# MEA of other model predictions
errors = lapply(models_list, function(x){
  (colMeans(abs(x[,1:12]-yout[,1])))
})%>% Reduce(f=cbind)
colnames(errors) = model_files

# MAE of accumulated predictions
rweacc = (colMeans(abs(rw[,13:15]-yout[,2:4])))
errorsacc = lapply(models_list, function(x){
  (colMeans(abs(x[,13:15]-yout[,2:4]),na.rm=TRUE))
})%>% Reduce(f=cbind)
colnames(errorsacc) = model_files


# final table with MAE of all models
res = rbind(errors, errorsacc)
resrw = rbind(cbind(rwe), cbind(rweacc))
res = cbind(res, resrw)
res_df = data.frame(res)

# average MAE
mean <- summarize_all(res_df, mean)
res_df <- rbind(res_df, mean)

# normalize w.r.t MSE random wals
res_mae_df <- res_df[,1:(ncol(res_df))]/res_df[, ncol(res_df)]

# max MAE
max <- summarize_all(res_mae_df, max)
res_mae_df <- rbind(res_mae_df, max)

# min MAE
min <- summarize_all(res_mae_df, min)
res_mae_df <- rbind(res_mae_df, min)

rownames(res_mae_df) <- c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12", "average", "max", "min")

# model with lower MAE for each horizon
res_mae_df$is_min <- names(res_mae_df)[apply(res_mae_df, MARGIN = 1, FUN = which.min)]

# export
write_xlsx(res_mae_df,"MAE.xlsx")


### SPA TEST WITH RMSE ###

# RMSE of random walk predictions
rwe = sqrt(colMeans((rw[,1:12]-yout[,1])^2))

# MSE of other model predictions
errors = lapply(models_list, function(x){
  sqrt(colMeans((x[,1:12]-yout[,1])^2))
})%>% Reduce(f=cbind)
colnames(errors) = model_files

# MSE of accumulated predictions
rweacc = sqrt(colMeans((rw[,13:15]-yout[,2:4])^2))
errorsacc = lapply(models_list, function(x){
  sqrt(colMeans((x[,13:15]-yout[,2:4])^2,na.rm=TRUE))
})%>% Reduce(f=cbind)
colnames(errorsacc) = model_files


# final table with MSE of all models
res = rbind(errors, errorsacc)
resrw = rbind(cbind(rwe), cbind(rweacc))
res = cbind(res, resrw)
res_df = data.frame(res)

# SPA of random walk predictions
spa_rw = hansen.spa(data.matrix(res_df[1:12,1:(ncol(res_df)-1)], rownames.force = NA), data.matrix(res_df[1:12,ncol(res_df)], rownames.force = NA),typeFunc=1,B=1000,geomMean=10,bandwidth=0.5, alpha=0.05, k=1, gamma=0.1)[2]

# SPA of other model predictions
SPA = lapply(model_files, function(x){hansen.spa(data.matrix(res_df[1:12,1:(ncol(res_df)-1)], rownames.force = NA),data.matrix(res_df[1:12,ncol(res_df)], rownames.force = NA),typeFunc=1,B=1000,geomMean=4,bandwidth=0.5, alpha=0.05, k=1, gamma=0.1)}[2])%>% Reduce(f=cbind)


# final table with p-values of all models
res_SPA = cbind(spa_rw, SPA)
res_SPA_df = data.frame(res_SPA)
colnames(res_SPA_df) = c("RW", model_files)

# export
write_xlsx(res_SPA_df,"SPA_sq.xlsx")