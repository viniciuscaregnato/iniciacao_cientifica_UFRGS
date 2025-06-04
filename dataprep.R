dataprep = function(ind, df, variable, horizon, add_dummy = TRUE, nlags=4)
  
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
  
  x=df                                               # as variveis explicativas sao todo o df do momento
  
  
  X=embed(as.matrix(x),nlags)                        # X é a matriz x, com nlags defasagens
  
  Xin=X[-c((nrow(X)-horizon+1):nrow(X)),]            # remove as ultimas horizon linhas de X
  Xout=X[nrow(X),]                                   # armazena em Xout os valores da ultima linha da matriz X, em forma de vetor
  Xout=t(as.vector(Xout))                            # garante que Xout seja de fato uma linha, n apenas um vetor
  yin=tail(y,nrow(Xin))                              # pega os últimos nrow(Xin) valores do vetor y
  
  
  
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
  
}
