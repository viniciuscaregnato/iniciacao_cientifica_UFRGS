# Após salvar no enviroment os códigos, este script rodará as rotinas

library(tidyverse)
library(randomForest)

load("data.rda")

# é necessário tranformar a coluna data como nomes das linhas

View(data)

dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)




rolling_window(runrf, data, nwindow=2, horizon = 2,variable = "CPIAUCSL", add_dummy = TRUE, nlags=12)

