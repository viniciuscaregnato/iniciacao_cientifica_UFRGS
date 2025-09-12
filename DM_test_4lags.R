library(forecast)

load("forecasts_4lags/yout.rda")
load("forecasts_4lags/rw.rda")

model_files = c(setdiff(list.files("forecasts_4lags/"), "yout.rda"))

models_list = list()
for(i in 1:(length(model_files)-1)){
  
  load(paste("forecasts_4lags/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_4lags
  
}
models_list[[(length(models_list)+1)]] = rw

names(models_list) = model_files
View(models_list)


errors_list_h1_4lags = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,1]}))
colnames(errors_list_h1_4lags) = model_files

errors_list_h3_4lags = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,3]}))
colnames(errors_list_h3_4lags) = model_files

errors_list_h6_4lags = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,6]}))
colnames(errors_list_h6_4lags) = model_files

errors_list_h12_4lags = Reduce(cbind,lapply(models_list, function(x){yout[,1] - x[,12]}))
colnames(errors_list_h12_4lags) = model_files

View(errors_list_h1_4lags)
View(errors_list_h3_4lags)
View(errors_list_h6_4lags)
View(errors_list_h12_4lags)

save(errors_list_h1_4lags, file="errors_lists_4lags/errors_list_h1.rda")
save(errors_list_h3_4lags, file="errors_lists_4lags/errors_list_h3.rda")
save(errors_list_h6_4lags, file="errors_lists_4lags/errors_list_h6.rda")
save(errors_list_h12_4lags, file="errors_lists_4lags/errors_list_h12.rda")


# RW x AR ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,1], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,1], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,1], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,1], h = 12, power = 2, alternative = "two.sided")

# RW x CWB ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,2], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,2], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,2], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,2], h = 12, power = 2, alternative = "two.sided")

# RW x CWB AR ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,3], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,3], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,3], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,3], h = 12, power = 2, alternative = "two.sided")




# RW x ELASTIC NET ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,4], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,4], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,4], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,4], h = 12, power = 2, alternative = "two.sided")

# RW x LASSO ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,5], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,5], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,5], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,5], h = 12, power = 2, alternative = "two.sided")

# RW x RF ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,6], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,6], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,6], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,6], h = 12, power = 2, alternative = "two.sided")


# RW x RIDGE ####
#h=1
dm.test(errors_list_h1_4lags[,8], errors_list_h1_4lags[,7], h = 1, power = 2, alternative = "two.sided")

#h=3
dm.test(errors_list_h3_4lags[,8], errors_list_h3_4lags[,7], h = 3, power = 2, alternative = "two.sided")

#h=6
dm.test(errors_list_h6_4lags[,8], errors_list_h6_4lags[,7], h = 6, power = 2, alternative = "two.sided")

#h=12
dm.test(errors_list_h12_4lags[,8], errors_list_h12_4lags[,7], h = 12, power = 2, alternative = "two.sided")





# CWB x LASSO ####

# h=1
dm.test(errors_list_h1_4lags[,2], errors_list_h1_4lags[,4], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3_4lags[,2], errors_list_h3_4lags[,4], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6_4lags[,2], errors_list_h6_4lags[,4], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,2], errors_list_h12_4lags[,4], h=12,  power = 2, alternative = "two.sided")


# CWB X AR ####

# h=1
dm.test(errors_list_h1_4lags[,2], errors_list_h1_4lags[,1], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,2], errors_list_h3_4lags[,1], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6_4lags[,2], errors_list_h6_4lags[,1], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,2], errors_list_h12_4lags[,1], h=12,  power = 2, alternative = "two.sided")


# CWB x ELASTIC ####

# h=1
dm.test(errors_list_h1_4lags[,2], errors_list_h1_4lags[,4], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,2], errors_list_h3_4lags[,4], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6_4lags[,2], errors_list_h6_4lags[,4], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,2], errors_list_h12_4lags[,4], h=12,  power = 2, alternative = "two.sided")


# CWB x RANDOM FOREST ####

# h=1
dm.test(errors_list_h1_4lags[,2], errors_list_h1_4lags[,6], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,2], errors_list_h3_4lags[,6], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6_4lags[,2], errors_list_h6_4lags[,6], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,2], errors_list_h12_4lags[,6], h=12,  power = 2, alternative = "two.sided")


# CWB x RIDGE ####

# h=1
dm.test(errors_list_h1_4lags[,2], errors_list_h1_4lags[,7], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,2], errors_list_h3_4lags[,7], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6_4lags[,2], errors_list_h6_4lags[,7], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,2], errors_list_h12_4lags[,7], h=12,  power = 2, alternative = "two.sided")







# RANDOM FOREST x AR ####

# h=1
dm.test(errors_list_h1_4lags[,6], errors_list_h1_4lags[,1], h=1,  power = 2, alternative = "two.sided")


# h=3
dm.test(errors_list_h3_4lags[,6], errors_list_h3_4lags[,1], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6_4lags[,6], errors_list_h6_4lags[,1], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,6], errors_list_h12_4lags[,1], h=12,  power = 2, alternative = "two.sided")


# RANDOM FOREST x ELASTIC NET ####

# h=1
dm.test(errors_list_h1_4lags[,6], errors_list_h1_4lags[,4], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,6], errors_list_h3_4lags[,4], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6_4lags[,6], errors_list_h6_4lags[,4], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,6], errors_list_h12_4lags[,4], h=12,  power = 2, alternative = "two.sided")

# RANDOM FOREST x LASSO ####

# h=1
dm.test(errors_list_h1_4lags[,6], errors_list_h1_4lags[,5], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,6], errors_list_h3_4lags[,5], h=3,  power = 2, alternative = "two.sided")

# h=6
dm.test(errors_list_h6_4lags[,6], errors_list_h6_4lags[,5], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,6], errors_list_h12_4lags[,5], h=12,  power = 2, alternative = "two.sided")



# RANDOM FOREST x RIDGE ####

# h=1
dm.test(errors_list_h1_4lags[,6], errors_list_h1_4lags[,7], h=1,  power = 2, alternative = "two.sided")

# h=3
dm.test(errors_list_h3_4lags[,6], errors_list_h3_4lags[,7], h=3,  power = 2, alternative = "two.sided")


# h=6
dm.test(errors_list_h6_4lags[,6], errors_list_h6_4lags[,7], h=6,  power = 2, alternative = "two.sided")

# h=12
dm.test(errors_list_h12_4lags[,6], errors_list_h12_4lags[,7], h=12,  power = 2, alternative = "two.sided")

