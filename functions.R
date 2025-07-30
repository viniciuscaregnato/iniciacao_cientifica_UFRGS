# dataprep ####

dataprep = function(ind, df, variable, horizon = horizon, add_dummy = TRUE, univar = FALSE, nlags=nlags)
  
{
  
  df=df[ind,]
  y=df[,variable]                                    
  
  
  if(univar==FALSE){
    factors=princomp(scale(df))$scores[,1:4]
    x=cbind(df,factors)
    x=as.data.frame(x)
    X=f_add_lags(x, nlags)
  }else{
    x = as.matrix(df[,variable])                   
    X=embed(as.matrix(x),nlags)
  }
  
  X=as.matrix(X)
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            
  Xout=X[nrow(X),]                                   
  Xout=t(as.vector(Xout))                            
  yin=tail(y,nrow(Xin))                              
  
  
  
  if("2008-11-01" %in% names(yin)){                  
    
    dummy=rep(0,length(yin))                         
    intervention=which(names(yin)=="2008-11-01")     
    dummy[intervention]=1                            
    if(add_dummy == TRUE){                            
      Xin=cbind(Xin,dummy)                           
      Xout=cbind(Xout,0)                             
    }
    
  }else{                                             
    dummy = rep(0,length(yin))                       
    if(add_dummy == TRUE){                           
      Xin=cbind(Xin,dummy)                           
      Xout=cbind(Xout,0)                             
    }
  }
  
  return(list(dummy = dummy, Xin = Xin, Xout = Xout, yin = yin))
  
}



# autoregressive runar ####

runar=function(ind,df,variable = variable,horizon = horizon, type = "bic", nlags=nlags){
  prep_data = dataprep(ind,df,variable,horizon, add_dummy = FALSE, univar = TRUE, nlags = nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  if(type=="bic"){
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
  }
  coef=coef(modelest)                            
  coef[is.na(coef)] = 0                         
  forecast=c(1,Xout[,1:best],0)%*%coef               
  
  return(list(forecast=forecast))
}


#  random forest ####

runrf=function(ind,df,variable,horizon, nlags = nlags)
  
  
{
  prep_data = dataprep(ind,df,variable,horizon, nlags = nlags)     
  Xin = prep_data$Xin                               
  yin = prep_data$yin                               
  Xout = prep_data$Xout                             
  
  modelest=randomForest::randomForest(Xin,yin, importance = TRUE)
  
  
  forecast=predict(modelest,Xout)                           
  
  ## outputs
  importance = randomForest::importance(modelest)    
  outputs = list(importance = importance)           
  
  return(list(forecast=forecast, outputs = outputs))
  
}


# component wise boosting ####

runcwb=function(ind,df,variable = variable,horizon = horizon, nlags=nlags, lrate = 0.1, nite = 100){
  
  prep_data=dataprep(ind, df, variable, horizon = horizon, add_dummy = TRUE, univar = FALSE, nlags=nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
 modelest= glmboost(
    y = yin,
    x = Xin,
    offset = 0,
    center = TRUE,
    control = boost_control(
      mstop = nite,
      nu = lrate
    )
  )
  
  aic = AIC(modelest, method = "corrected")
  aic_seq = attributes(aic)$AIC
  m_opt = min(
    c(
      which(diff(aic_seq) > 0)[1],
      which.min(aic_seq)
    ),
    na.rm = TRUE
  )
  
  modelest = modelest[m_opt]
  
  
  coef_opt_aux = mboost::extract(modelest, what = "coefficients")
  coef_opt = rep(0, ncol(Xin))
  names(coef_opt) = colnames(Xin)
  coef_opt[names(coef_opt_aux)] = coef_opt_aux
  
  #forecast
  
  Xin_mean = as.vector(apply(Xin, 2, mean))
  yin_mean=mean(yin)
  
  forecast = sum(((Xout-Xin_mean)*coef_opt))+yin_mean
  
  outputs=list(coef_opt)
  
  
  
  
  return(list(
    forecast=forecast,
    outputs=outputs
  )
  )
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

# name lags ####

f_add_lags_aux <- function(
    x, # vetor com os valores numéricos
    nlags, # lag máx. desejado
    name # nome a ser colocado no vetor resultante
) {
  result <- embed(x, nlags) %>% data.frame()
  colnames(result) <- c(
    paste(name, "_t_", 1:nlags, sep = "")
  )
  
  return(result)
}

f_add_lags <- function(
    x, # data.frame com colunas nomeadas
    nlags # lag máx. desejado
) {
  
  x=data.frame(x)
  
  if (!is.data.frame(x)) {
    stop(
      "x must be a data.frame"
    )
  }
  
  dados <- data.frame(
    temp = rep(1, nrow(x) - nlags + 1)
  )
  name_aux <- colnames(x)
  
  for (i in seq_len(ncol(x))) {
    dados <- data.frame(
      dados,
      f_add_lags_aux(
        x = x[, i],
        nlags = nlags,
        name = name_aux[i]
      )
    )
  }
  dados_aux <- data.frame(dados[, -1])
  colnames(dados_aux) <- colnames(dados)[-1]
  return(dados_aux)
}
