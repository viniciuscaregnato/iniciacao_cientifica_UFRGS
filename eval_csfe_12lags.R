library(tidyverse)

load("forecasts_12lags/yout.rda")
load("forecasts_12lags/rw.rda")

model_files = setdiff(list.files("forecasts_12lags/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts_12lags/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_12lags
  
}
names(models_list) = model_files

# CSFE h=1 ####
errors_h1 = lapply(models_list, function(x){
  cumsum((rw[,1]-yout[,1])^2)-((x[,1]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(errors_h1) = model_files

View(errors_h1)

matplot(1:nrow(errors_h1),  errors_h1, type="l", col=1:4, lty=1,
        xlab="Data", ylab="CSFE")

# CSFE h=3 ####
errors_h3 = lapply(models_list, function(x){
  cumsum((rw[,3]-yout[,1])^2)-((x[,3]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(errors_h3) = model_files

matplot(1:nrow(errors_h3),  errors_h3, type="l", col=1:4, lty=1,
        xlab="Data", ylab="CSFE")

# CSFE h=6 ####
errors_h6 = lapply(models_list, function(x){
  cumsum((rw[,6]-yout[,1])^2)-((x[,6]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(errors_h6) = model_files

matplot(1:nrow(errors_h6),  errors_h6, type="l", col=1:4, lty=1,
        xlab="Data", ylab="CSFE")

# CSFE h=12 ####
errors_h12 = lapply(models_list, function(x){
  cumsum((rw[,12]-yout[,1])^2)-((x[,12]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(errors_h12) = model_files

matplot(1:nrow(errors_h12),  errors_h12, type="l", col=1:4, lty=1,
        xlab="Data", ylab="CSFE")


save(errors_h1,file = "csfe_results/h1_12lags.rda")
save(errors_h3,file = "csfe_results/h3_12lags.rda")
save(errors_h6,file = "csfe_results/h6_12lags.rda")
save(errors_h12,file = "csfe_results/h12_12lags.rda")
