runrf=function(ind,df,variable,horizon, add_dummy = TRUE, nlags=4)
  
  request("randomForest")
  
  # ind: indices das linhas q serao usadas, recebido pelo apply, que extraiu da indmat
  # df: o dataframe
  # variable: a variavel dependente
  # horizon: o horizonte de previsao
  
{
  prep_data = dataprep(ind,df,variable,horizon)     # arquivo configurado conforme o dataprep informado
  Xin = prep_data$Xin                               # remove as linhas conforme o horizon
  yin = prep_data$yin                               # ajusta o numero de linhas igual Xin
  Xout = prep_data$Xout                             # adiciona uma coluna de zeros (dummies)
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)  # aplica rf aos parametros e cria o modelo modelest
  forecast=predict(modelest,Xout)                   # aplica modelest aos dados Xout        
  
  ## outputs
  importance = randomForest::importance(modelest)   # acessa as variaveis mais importantes de modelest 
  outputs = list(importance = importance)           # lista as variaveis mais importantes
  
  return(list(forecast=forecast, outputs = outputs))
  
  #retona 1) a previsao e 2) a lista de variaveis mais importantes
}
