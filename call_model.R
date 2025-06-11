# Após salvar no enviroment os códigos, este script rodará as rotinas

load("data.rda")

View(data)

dates = data$date
data = data%>%select(-date)%>%as.matrix()
rownames(data) = as.character(dates)

rolling_window(runrf, data, nwindow=2, horizon = 2,variable = "CPIAUCSL", add_dummy = TRUE, nlags=12)

