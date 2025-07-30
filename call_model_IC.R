library(devtools)


# install_github("gabrielrvsc/HDeconometrics", force = TRUE)
library(HDeconometrics)
library(glmnet)
library(randomForest)
library(dplyr)
library(mboost)


source("functions/rolling_window.R")
source("functions/functions.R")


model_name <- "AR"
model_function <- runar



load("data/rawdata.RData")
data<-dados
colnames(data)[1]="CPIAUCSL"

#View(data)
#dim(data)

#dates = data$date
#data = data%>%select(-date)%>%as.matrix()
#rownames(data) = as.character(dates)

nwindows = 312
lags_list = list()
model_list = list() 

lags = c(4,12)


for (j in lags) {
  cat(j,"lags:", "\n")
  contador <- match(j, lags)
  for(i in 1:12){
    model = rolling_window(model_function,data,nwindows+i-1,i,"CPIAUCSL", nlags=j) 
    model_list[[i]] = model                                                                     
    cat(i,"\n")
}
  lags_list[[contador]] = model_list
}


forecasts_4lags = Reduce(cbind, lapply(lags_list[[1]], function(x)head(x$forecast,nwindows)))
forecasts_12lags = Reduce(cbind,lapply(lags_list[[2]], function(x)head(x$forecast,nwindows)))



# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses
forecasts_4lags = accumulate_model(forecasts_4lags)
forecasts_12lags = accumulate_model(forecasts_12lags)


View(forecasts_12lags)
View(forecasts_4lags)



save(forecasts_4lags,file = paste("forecasts_4lags/",model_name,".rda",sep = ""))

save(forecasts_12lags,file = paste("forecasts_12lags/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],312),type = "l")
lines(forecasts_12lags[,1],col = 3)
lines(forecasts_4lags[,1],col = 4)
