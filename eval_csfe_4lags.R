library(tidyverse)

# organizando dados

load("forecasts_4lags/yout.rda")
load("forecasts_4lags/rw.rda")

model_files = setdiff(list.files("forecasts_4lags/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts_4lags/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_4lags
  
}
names(models_list) = model_files

# CSFE h=1 ####
CSFE_h1_4lags = lapply(models_list, function(x){
  cumsum((rw[,1]-yout[,1])^2)-((x[,1]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(CSFE_h1_4lags) = model_files

View(CSFE_h1_4lags)

matplot(1:nrow(CSFE_h1_4lags),  CSFE_h1_4lags, type="l", col=length(model_files), lty=1,
        xlab="Data", ylab="CSFE_h1_4lags")

# CSFE h=3 ####
CSFE_h3_4lags = lapply(models_list, function(x){
  cumsum((rw[,3]-yout[,1])^2)-((x[,3]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(CSFE_h3_4lags) = model_files

matplot(1:nrow(CSFE_h3_4lags),  CSFE_h3_4lags, type="l", col=length(model_files), lty=1,
        xlab="Data", ylab="CSFE_h3_4lags")

# CSFE h=6 ####
CSFE_h6_4lags = lapply(models_list, function(x){
  cumsum((rw[,6]-yout[,1])^2)-((x[,6]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(CSFE_h6_4lags) = model_files

matplot(1:nrow(CSFE_h6_4lags),  CSFE_h6_4lags, type="l", col=length(model_files), lty=1,
        xlab="Data", ylab="CSFE_h6_4lags")

# CSFE h=12 ####
CSFE_h12_4lags = lapply(models_list, function(x){
  cumsum((rw[,12]-yout[,1])^2)-((x[,12]-yout[,1])^2)
})%>% Reduce(f=cbind)
colnames(CSFE_h12_4lags) = model_files

matplot(1:nrow(CSFE_h12_4lags),  CSFE_h12_4lags, type="l", col=length(model_files), lty=1,
        xlab="Data", ylab="CSFE_h12_4lags")


save(CSFE_h1_4lags,file = "csfe_results/h1_4lags.rda")
save(CSFE_h3_4lags,file = "csfe_results/h3_4lags.rda")
save(CSFE_h6_4lags,file = "csfe_results/h6_4lags.rda")
save(CSFE_h12_4lags,file = "csfe_results/h12_4lags.rda")


# Para visualizar as curvas ####

load("csfe_results/h12_4lags.rda")
matplot(1:nrow(CSFE_h12_4lags),  CSFE_h12_4lags, type="l", col=length(model_files), lty=1,
        xlab="Data", ylab="CSFE_h12_4lags")
