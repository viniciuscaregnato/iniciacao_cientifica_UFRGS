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

nwindows = 4
model_list = list() 

for(i in 1:12){
  k=0
  model_list[[i]] = list()
  for (j in c(4, 12)) {
  model = rolling_window(runrf,data,nwindows+i-1,i,"CPIAUCSL", add_dummy = TRUE, nlags=j)    # 
  model_list[[i]][[k=k+1]] = model                                                                    # 
  cat(i,"\n")
}
}

# revisar o for j, verificar o que fica salvo

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))




# o accumulate_model calcula as diagonais, sendo assim, os valores de previsao de 3 e 6 me
forecasts = accumulate_model(forecasts)

save(forecasts,file = paste("forecasts/",model_name,".rda",sep = ""))


plot(tail(data[,"CPIAUCSL"],180),type = "l")
lines(forecasts[,1],col = 3)
