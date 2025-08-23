library(forecast)

load("forecasts_4lags/yout.rda")
load("forecasts_4lags/rw.rda")

model_files = setdiff(list.files("forecasts_4lags/"),c("rw.rda", "yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts_4lags/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_4lags
  
}
names(models_list) = model_files

View(models_list)

errors_list_h1 = lapply(models_list, function(x){yout[,1] - x[,1]})
errors_list_h1$rw = yout[,1] - rw[,1]
errors_list_h1 = errors_list_h1%>% Reduce(f=cbind)

colnames(errors_list_h1) = c(model_files, "rw")

View(errors_list_h1)

"parei ao tentar criar uma matriz com as series residuais e preciso adicionar a serie residual de rw,
agora tenho que fazer uma errors_list_h para cada horizon"






errors_list_h3 = lapply(models_list, function(x){yout[,1] - x[,3]})%>% Reduce(f=cbind)
colnames(errors_list_h3) = model_files

errors_list_h6 = lapply(models_list, function(x){yout[,1] - x[,6]})%>% Reduce(f=cbind)
colnames(errors_list_h6) = model_files

errors_list_h12 = lapply(models_list, function(x){yout[,1] - x[,12]})%>% Reduce(f=cbind)
colnames(errors_list_h12) = model_files

View(errors_list_h1)
View(errors_list_h3)
View(errors_list_h6)
View(errors_list_h12)

# RW x AR ####
#h=1
dm.test(errors_list_h1[,7], errors_list_h1[,1], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,7], errors_list_h3[,1], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,7], errors_list_h6[,1], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,7], errors_list_h12[,1], h = 12, power = 2, alternative = "two.sided")

# RW x CWB ####
#h=1
dm.test(errors_list_h1[,7], errors_list_h1[,2], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,7], errors_list_h3[,2], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,7], errors_list_h6[,2], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,7], errors_list_h12[,2], h = 12, power = 2, alternative = "two.sided")

# RW x ELASTIC NET ####
#h=1
dm.test(errors_list_h1[,7], errors_list_h1[,3], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,7], errors_list_h3[,3], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,7], errors_list_h6[,3], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,7], errors_list_h12[,3], h = 12, power = 2, alternative = "two.sided")




# RW x LASSO ####
#h=1
dm.test(errors_list_h1[,7], errors_list_h1[,4], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,7], errors_list_h3[,4], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,7], errors_list_h6[,4], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,7], errors_list_h12[,4], h = 12, power = 2, alternative = "two.sided")

# RW x RANDOM FOREST ####
#h=1
dm.test(errors_list_h1[,7], errors_list_h1[,5], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,7], errors_list_h3[,5], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,7], errors_list_h6[,5], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,7], errors_list_h12[,5], h = 12, power = 2, alternative = "two.sided")

# RW x RIDGE ####
#h=1
dm.test(errors_list_h1[,7], errors_list_h1[,6], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3[,7], errors_list_h3[,6], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6[,7], errors_list_h6[,6], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12[,7], errors_list_h12[,6], h = 12, power = 2, alternative = "two.sided")
