## rolling window function ##

# inputs
#   - fn: function applied for the forecast - from functions.R script
#   - df: dataset 
#   - nwindow: number of windows (1 by default)
#   - horizon: time horizon for the forecast
#   - variable: variable to predict (y)
# 
# outputs
#   - forecast: predicted inflation value made by the model
#   - outputs: parameters of the model#


rolling_window=function(fn,df,nwindow=1,horizon,variable,...){
  
  ind = 1:nrow(df)
  window_size = nrow(df)-nwindow 
  indmat = matrix(NA,window_size,nwindow)
  indmat[1,] = 1:ncol(indmat)
  for(i in 2:nrow(indmat)){
    indmat[i,]=indmat[i-1,]+1
  }
    
  rw = apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...) # estimation of the model on each column of indmat
  forecast = unlist(lapply(rw,function(x)x$forecast))
  outputs = lapply(rw,function(x)x$outputs)
  return(list(forecast=forecast, outputs=outputs))
  
}