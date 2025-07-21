library(devtools)


# install_github("gabrielrvsc/HDeconometrics", force = TRUE)
library(HDeconometrics)
library(glmnet)
library(randomForest)
library(dplyr)
library(mboost)


source("functions/rolling_window.R")
source("functions/functions.R")


model_name <- "ARrr"
model_function <- runar



load("data/data.rda")

View(data)

dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

nwindows = 180
lags_list = list()
model_list = list() 

lags = c(4,12)


for (j in lags) {
  contador <- match(j, lags)
  for(i in 1:12){
    model = rolling_window(model_function,data,nwindows+i-1,i,"CPIAUCSL", nlags=j) 
    model_list[[i]] = model                                                                     
    cat(i,"\n")
}
  lags_list[[contador]] = model_list
    cat(j, "\n")
}


forecasts_h4 = Reduce(cbind, lapply(lags_list[[1]], function(x)head(x$forecast,nwindows)))
forecasts_h12 = Reduce(cbind,lapply(lags_list[[2]], function(x)head(x$forecast,nwindows)))



# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses
forecasts_h4 = accumulate_model(forecasts_h4)
forecasts_h12 = accumulate_model(forecasts_h12)


View(forecasts_h12)
View(forecasts_h4)



save(forecasts_h4,file = paste("forecasts_h4/",model_name,".rda",sep = ""))

save(forecasts_h12,file = paste("forecasts_h12/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],180),type = "l")
lines(forecasts_h12[,1],col = 3)
lines(forecasts_h4[,1],col = 4)
