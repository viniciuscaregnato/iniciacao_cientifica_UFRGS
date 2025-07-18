# dataprep

dataprep = function(ind, df, variable, horizon = horizon, add_dummy = TRUE, univar = FALSE, factonly=FALSE, nofact = FALSE, nlags=nlags)
  
{
  
  df=df[ind,]
  y=df[,variable]                                    
  
  
  if (nofact==TRUE){                                # nao se observa NOFACT = TRUE no meu algoritmo
    if(univar==FALSE){
      x=df
    }else{
      x=as.matrix(df[,variable])
    }
    
    
    
  }else{
    
    
    if(univar==FALSE){
      factors=princomp(scale(df))$scores[,1:4]
      x=cbind(df,factors)                            
    }else{
      x = as.matrix(df[,variable])                   
    }
    
  }
  
  
  
  X=embed(as.matrix(x),nlags)                        
  
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

runar=function(ind,df,variable = variable,horizon = horizon, type = "fixed", nlags=nlags){
  prep_data = dataprep(ind,df,variable,horizon, add_dummy = FALSE, univar = TRUE, nlags = nlags)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  dummy = prep_data$dummy
  
  
  if(type=="fixed"){
    modelest=lm(yin~Xin+dummy)
    best = ncol(Xin)                        
  }
  
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
