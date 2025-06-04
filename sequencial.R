df <- read.csv("dados")
View(dados)


# ROLLING WINDOW ####

fn <- 
df <- df 
nwindow <- 10
horizon <- 1
variable <- "y"
add_dummy = TRUE
nlags=2 

# ROLLING WINDOW: PARTE 1 ####


  ind=1:nrow(df)                                     # ind: indices das linhas que serao usadadas do dataframe ao aplicar df=df[ind,]
  window_size=nrow(df)-nwindow                       # window_size: tamanho da janela insample
  indmat=matrix(NA,window_size,nwindow)              # cria uma matriz indmat de tamanho (window_size × nwindow) (indices de insample x janelas de previsao)
  indmat[1,]=1:ncol(indmat)                          # primeira linha são os indices iniciais de cada janela
  for(i in 2:nrow(indmat)){                          # demais indices de df para cada janela sao preenchidas
    indmat[i,]=indmat[i-1,]+1                        #
  }


  View(indmat)
  
# ROLLING WINDOW: PARTE 2 ####
  
  rw=apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...)
  
  ## aqui estamos aplicando a função fn sobre cada coluna(janela) de indmat ##
  
  # indmat: a matriz que contem as janelas
  # 2: função será aplicada nas colunas(2), nao nas linhas(1)
  # fn: função que será aplicada
  # df= dataframe
  # horizon: horizonte de previsao
  # variable: variavel alvo
  
  ## apply retorna os resultados e previsoes das janelas, ou seja,
  ## rw <- list(
  ## list(forecast = ..., results = ...),  # resultado da 1ª coluna(janela)
  ## list(forecast = ..., results = ...),  # resultado da 2ª coluna(janela)
  ## list(forecast = ..., results = ...)   # resultado da 3ª coluna(janela)
  ##          ) 
  
  
# DATAPREP: TRATANDO A PRIMEIRA JANELA ####
   #dataprep é chamada pela funcao#
  
  ind <- indmat[,1]
  
  
  df=df[ind,]                                        # define as linhas de df conformeo indice indicado pela coluna de indmat
  y=df[,variable]                                    # separa coluna de y neste df do momento
  x=df                                               # as variveis explicativas sao todo o df do momento
  
  
  X=embed(as.matrix(x),nlags)                        # X é a matriz x, com nlags defasagens
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            # remove as ultimas horizon linhas de X
  Xout=X[nrow(X),]                                   # armazena em Xout os valores da ultima linha da matriz X, em forma de vetor
  Xout=t(as.vector(Xout))                            # garante que Xout seja de fato uma linha, n apenas um vetor
  yin=tail(y,nrow(Xin))                              # pega os últimos nrow(Xin) valores do vetor y
  
  
  View(x)
  View(X)
  View(Xin)
  View(yin)
  

  
# DATAPREP: TRATANDO A SEGUNDA JANELA ####
   #dataprep é chamada pela funcao#
  
  ind <- indmat[,2]
  
  
  df=df[ind,]                                        # define as linhas de df conformeo indice indicado pela coluna de indmat
  y=df[,variable]                                    # separa coluna de y neste df do momento
  x=df                                               # as variveis explicativas sao todo o df do momento
  
  
  X=embed(as.matrix(x),nlags)                        # X é a matriz x, com nlags defasagens
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            # remove as ultimas horizon linhas de X
  Xout=X[nrow(X),]                                   # armazena em Xout os valores da ultima linha da matriz X, em forma de vetor
  Xout=t(as.vector(Xout))                            # garante que Xout seja de fato uma linha, n apenas um vetor
  yin=tail(y,nrow(Xin))                              # pega os últimos nrow(Xin) valores do vetor y
  
  
  View(x)
  View(X)
  View(Xin)
  View(yin)  
  
  
# DATAPREP: LIDANDO COM DUMMY E DEFININDO VARIAVEIS ####
  if("2008-11-01" %in% names(yin)){                  # (se 2008-11-01 está no names(yin):
    
    dummy=rep(0,length(yin))                         # cria um vetor de zeros, chamado dummy, com a extensao de yin
    intervention=which(names(yin)=="2008-11-01")     # "intervention" apenas guarda o valor do indice de "2008-11-01"
    dummy[intervention]=1                            # tona a variavel "dummy" igual a 1, para o indice de internvention
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
  
  
