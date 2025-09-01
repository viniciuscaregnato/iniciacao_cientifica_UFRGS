library(tidyverse)

load("forecasts_4lags/yout.rda")
load("forecasts_4lags/rw.rda")

model_files = setdiff(list.files("forecasts_4lags/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts_4lags/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_4lags
  
}
names(models_list) = model_files

rwe = sqrt(colMeans((rw[,1:12]-yout[,1])^2))

errors = Reduce(cbind,lapply(models_list, function(x){
  sqrt(colMeans((x[,1:12]-yout[,1])^2))
}))

colnames(errors) = model_files


rweacc = sqrt(colMeans((rw[,13:15]-yout[,2:4])^2))

errorsacc = Reduce(cbind,lapply(models_list, function(x){
  sqrt(colMeans((x[,13:15]-yout[,2:4])^2,na.rm=TRUE))
}))
colnames(errorsacc) = model_files


vecs_errors = list()
for(i in 1:length(models_list)){
  
  vec = c(errors[,i], errorsacc[,i])
  vecs_errors[[i]] = vec
  
  vec_rwerrors = c(rwe,rweacc)
  
}
mat_errors=do.call(rbind, vecs_errors)

res_4lags=matrix(NA, nrow=nrow(mat_errors), ncol=ncol(mat_errors))
for(i in 1:nrow(mat_errors)){
  row=mat_errors[i,]/vec_rwerrors
  res_4lags[i,]=row
  
}

colnames(res_4lags)=colnames(mat_errors)
rownames(res_4lags)=names(models_list)

View(res_4lags)

save(res_4lags, file = "RMSE_ratios_results/res_4lags.rda" )

# acessando resultados

load("RMSE_ratios_results/res_4lags.rda" )
View(res_4lags)
