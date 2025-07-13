rolling_window=function(fn,df,nwindow,horizon,variable, add_dummy = TRUE, nlags = 4)
  
  # fn: função que será aplicada à rolling window
  # df: dataframe
  # nwindow: numero de previsoes, out of sample
  # horizon: horizonte de previsao
  # variable: variavel alvo
  # ...: demais paramertos que podem ser inclusos
  
  
{
  ind=1:nrow(df)                                     # ind: indices das linhas que serao usadadas do dataframe ao aplicar df=df[ind,]
  window_size=nrow(df)-nwindow                       # window_size: tamanho da janela insample
  indmat=matrix(NA,window_size,nwindow)              # cria uma matriz indmat de tamanho (window_size × nwindow) (indices de insample x numero da janela)
  indmat[1,]=1:ncol(indmat)                          # primeira linha são os indices iniciais de cada janela
  for(i in 2:nrow(indmat)){                          # demais indices de df para cada janela sao preenchidas
    indmat[i,]=indmat[i-1,]+1                        #
  }
  
  
  rw=apply(indmat,2,fn,df=df,horizon=horizon,variable=variable, add_dummy = TRUE, nlags)
  
  ## aqui estamos aplicando a função sobre cada coluna(janela) de indmat ##
  
  # indmat: a matriz que contem as janelas
  # 2: função será aplicada nas colunas(2), nao nas linhas(1)
  # fn: função que será aplicada
  # df= dataframe (argumetno de fn)
  # horizon: horizonte de previsao (argumetno de fn)
  # variable: variavel alvo (argumetno de fn)
  
  ## apply retorna os resultados e previsoes das janelas, ou seja,
  ## rw <- list(
  ## list[forecast = previsao da janela, outputs = list(variavel1, variavel2, ..)],  # resultado da 1ª coluna(janela)
  ## list[forecast = previsao da janela, outputs = list(variavel1, variavel2, ..)],  # resultado da 2ª coluna(janela)
  ## list[forecast = previsao da janela, outputs = list(variavel1, variavel2, ..)]   # resultado da 3ª coluna(janela)
  ##          ) 
  
  forecast=unlist(lapply(rw,function(x)x$forecast))
  
  ## lapply() aplica a função " function(x){x$forecast} " a cada sublista (list(forecast, outputs)) contidas em rw,
  ## assim, ele consegue extrair $forecasts de cada sublista. Afinal, rw nao tem nehum retorno $forecast, 
  ## pois retorna apena uma lista de janelas, as quais contêm forecast e outputs
  ## o unlist() apenas concatena todos as previsões extraídas em um unico vetor
  
  outputs=lapply(rw,function(x)x$outputs)
  
  ## lapply() aplica a função "function(x){x$outputs}" para cada sublistas (list(forecast, outputs)) contidas em rw,
  ## assim, ele extrai examente o retorno $outputs das subslistas. afinal, rw pode apenas retornar
  ## uma unica lista, que contem sublistas de forecast e outputs.  
  ## outputs entao, é um vetor de listas (cada lista uma janela) e em cada uma as principais variaveis
  
  return(list(forecast=forecast, outputs=outputs))
  
}

