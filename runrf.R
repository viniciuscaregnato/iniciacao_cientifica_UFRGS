runrf=function(ind,df,variable,horizon, add_dummy = TRUE, nlags = nlags)
  
  # ind: indices das linhas q serao usadas, recebido pelo apply, que extraiu da indmat
  # df: o dataframe
  # variable: a variavel dependente
  # horizon: o horizonte de previsao
  
{
  prep_data = dataprep(ind,df,variable,horizon, nlags = nlags)     # roda o dataprep
  Xin = prep_data$Xin                               # chama o Xin de dataprep
  yin = prep_data$yin                               # chama o yin de dataprep
  Xout = prep_data$Xout                             # chama o Xout de dataprep
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)  # modelest = "modelo estimado"
  # randomForest::randomForest() chama a função randomForest() sem chamar library(randomForest) 
  # importance salva as variaveis mais importantes atraves de analise de erro OOB e permuting variables
  
  forecast=predict(modelest,Xout)                   # aplica modelest aos dados Xout        
  
  ## outputs
  importance = randomForest::importance(modelest)   # acessa as variaveis mais importantes de modelest 
  outputs = list(importance = importance)           # lista as variaveis mais importantes
  
  return(list(forecast=forecast, outputs = outputs))
  
  #retona 1) a previsao e 2) a lista de variaveis mais importantes
}
