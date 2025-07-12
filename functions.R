# dataprep ####
dataprep = function(ind, df, variable, horizon, add_dummy = TRUE, univar = FALSE, nofact = FALSE, nlags)
  
  # ind: indices das linhas que serao usadadas do dataframe ao aplicar df=df[ind,]
  # df: um data.frame
  # variable: nome da variavel do dataframe a ser analisada, será o y
  # horizon: horizonte de previsao, que determina quantas últimas observações serão retiradas da matriz X ao criar Xin e Xout
  # add_dummy (padrao TRUE): adiciona coluna "dummy" para a variavel "2008-11-01" na matriz "Xin" e uma coluna de zeros à matriz "Xout"
  # univar (padrao FALSE): caso for TRUE, os dados df[,variabele] sao transformados na matriz X, tornado a serie apenas determinada pelas suas defasagens
  #                        caso for FALSE, dai depende de factonly e nofact
  # factonly (padrao FALSE): só existe se nofactor for FALSE, caso for TRUE, é usado apenas fatores(primeiros 4 componentes principais) como variaveis
  # nofact (padrao FALSE): se for TRUE, ignora a extração de componentes pricipais PCA, e usa apenas os dados do modelo
  
{
  
  df=df[ind,]                                        # define as linhas de df conformeo indice indicado pela coluna de indamt
  y=df[,variable]                                    # separa coluna de y neste df do momento
  
  
  if (nofact==TRUE){                                # nao se observa NOFACT = TRUE no meu algoritmo
    if(univar==FALSE){
      x=df
    }else{
      x=as.matrix(df[,variable])
    }
    
    
    
  }else{
  
  
  if(univar==FALSE){
    factors=princomp(scale(df))$scores[,1:4]
    x=cbind(df,factors)                            # se multivar + factor, default adiciona principal components na matriz X
  }else{
    x = as.matrix(df[,variable])                   # se univar  + factor, matriz X é apenas de target var
  }
  
  }



X=embed(as.matrix(x),nlags)                        # X é a matriz x, com nlags defasagens

Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            # remove as ultimas horizon linhas de X
Xout=X[nrow(X),]                                   # armazena em Xout os valores da ultima linha da matriz X, em forma de vetor
Xout=t(as.vector(Xout))                            # garante que Xout seja de fato uma linha, n apenas um vetor
yin=tail(y,nrow(Xin))                              # pega os últimos nrow(Xin) valores do vetor y



if("2008-11-01" %in% names(yin)){                  # (se 2008-11-01 está no names(yin):
  
  dummy=rep(0,length(yin))                         # cria um vetor de zeros, chamado dummy, com a extensao de yin
  intervention=which(names(yin)=="2008-11-01")     # "intervention" apenas guarda o indice de "2008-11-01"
  dummy[intervention]=1                            # tona o valor, no vetor "dummy", igual a 1, para o indice de internvention
  if(add_dummy == TRUE){                           # (ainda se, add_dumy == TRUE, 
    Xin=cbind(Xin,dummy)                           # adiciona coluna "dummy" à matriz "Xin"
    Xout=cbind(Xout,0)                             # adiciona uma coluna de zeros à matriz "Xout"))
  }
  
}else{                                             # (se 2008-11-01 nao está no names(yin):
  dummy = rep(0,length(yin))                       # cria um vetor de zeros, chamado dummy, com a extensao de yin
  if(add_dummy == TRUE){                           # (ainda, se add_dummy for TRUE na função:
    Xin=cbind(Xin,dummy)                           # adiciona coluna "dummy" à matriz "Xin"
    Xout=cbind(Xout,0)                             # adiciona uma coluna de zeros à matriz "Xout"))
  }
}

return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))

}


# autoregressive runrar ####

runar=function(ind,df,variable,horizon, type = "fixed"){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  # Xin é constituído apenas por "variable", embedded nlags vezes
  # nao é adicionado vetor de dummy na matriz X, apenas criado, se 2008-11-01 estiver no names(yin)
  # o Xout sao as ultimas observações da janela X, que coicidem com a ultima linha de Yin
  
  if(type=="fixed"){
    modelest=lm(yin~Xin+dummy)
    best = ncol(Xin)                        # best é o numero de colunas de Xin
  }
  
  "if(type=="bic"){
    bb=Inf
    best = 1
    for(i in seq(1,ncol(Xin),1)){
      m=lm(yin~Xin[,1:i]+dummy)
      crit=BIC(m)
      if(crit<bb){
        bb=crit
        modelest=m
        best = i
      }
    }
  }"
  coef=coef(modelest)                           # pega os coeficientes estimados na regressao 
  coef[is.na(coef)] = 0                         # coeficientes com valor NA recebem o valor zero
                                                # dummy pode ficar NA por multicolinearidade
  forecast=c(1,Xout[,1:best],0)%*%coef          # atualiza Xout para c(1,Xout[,1:best],0) e multiplica pelos coeficientes estiamdos     
                                                
  return(list(forecast=forecast))
}


#  random forest ####

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


# accumulate_model ####

accumulate_model = function(forecasts){
  
  acc3 = c(rep(NA,2),sapply(1:(nrow(forecasts)-2), function(x){
    prod(1+diag(forecasts[x:(x+2),1:3]))-1
  })) 
  acc6 = c(rep(NA,5),sapply(1:(nrow(forecasts)-5), function(x){
    prod(1+diag(forecasts[x:(x+5),1:6]))-1
  }))
  acc12 = c(rep(NA,11),sapply(1:(nrow(forecasts)-11), function(x){
    prod(1+diag(forecasts[x:(x+11),1:12]))-1
  }))
  
  forecasts = cbind(forecasts,acc3,acc6,acc12)
  colnames(forecasts) = c(paste("t+",1:12,sep = ""),"acc3","acc6","acc12")
  
  return(forecasts)
  
}
