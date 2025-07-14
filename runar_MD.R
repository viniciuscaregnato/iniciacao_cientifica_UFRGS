#observando embed


df <- read.csv("dados_q1_l7.csv")
df<-as.matrix(df)
View(df)

horizon = 2
nlags <- 3
variable <- "y"

#UNIVAR, LOGO:
y <- df[,variable]
X <- as.matrix(df[,variable])

X <- embed(X,nlags)
View(X)

#Xin
Xin <- X[-c((nrow(X)-horizon+1):nrow(X)),]
View(Xin)

#Yin
Yin <- tail(y, nrow(Xin))
View(Yin)

#Xout
Xout <- X[nrow(X),]
Xout <- t(as.vector(Xout))

#a dummy
dummy <- rep(0, length(Yin))

# a regressao

modelest <- lm(Yin~Xin+dummy)
best = ncol(Xin)


coef=coef(modelest)
coef[is.na(coef)]=0

concatenado <- c(1,Xout[,1:best],0)
concatenado

# a previsao
forecast <- concatenado*coef



