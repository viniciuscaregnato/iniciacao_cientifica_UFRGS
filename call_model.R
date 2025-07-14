library(devtools)


# install_github("gabrielrvsc/HDeconometrics", force = TRUE) ### ainda nao entedi o uso
library(HDeconometrics)
library(glmnet)
library(randomForest)
library(dplyr)
library(mboost)


source("functions/rolling_window.R")
source("functions/functions.R")


model_name <- "AR"
model_function <- runar



load("data.rda")

View(data)

dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

nwindows = 180
model_list = list() 

for(i in 1:12){
  model = rolling_window(model_function,data,nwindows+i-1,i,"CPIAUCSL")
  model_list[[i]] = model
  cat(i,"\n")
}


#cria matriz forecasts 

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))



# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 meses e adiciona as colunas referetnes
forecasts = accumulate_model(forecasts)


save(forecasts,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],180),type = "l")
lines(forecasts[,1],col = 3)
