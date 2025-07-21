library(tidyverse)

load("forecasts/yout.rda")
load("forecasts/rw.rda")

model_files = setdiff(list.files("forecasts_h4/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts_h4/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_h4
  
}
names(models_list) = model_files

rwe = sqrt(colMeans((rw[,1:12]-yout[,1])^2))

errors = lapply(models_list, function(x){
  sqrt(colMeans((x[,1:12]-yout[,1])^2))
})%>% Reduce(f=cbind)

colnames(errors) = model_files


rweacc = sqrt(colMeans((rw[,13:15]-yout[,2:4])^2))

errorsacc = lapply(models_list, function(x){
  sqrt(colMeans((x[,13:15]-yout[,2:4])^2,na.rm=TRUE))
})%>% Reduce(f=cbind)
colnames(errorsacc) = model_files

res_h4 = as.matrix(t(c(errors/rwe,
                    errorsacc/rweacc)))
View(res_h4)
