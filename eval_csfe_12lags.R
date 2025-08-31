# organizando dados ####

load("forecasts_12lags/yout.rda")
load("forecasts_12lags/rw.rda")

model_files = setdiff(list.files("forecasts_12lags/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts_12lags/",model_files[i],sep = ""))
  models_list[[i]] = forecasts_12lags
  
}
names(models_list) = model_files

# CSFE h=1 ####
CSFE_h1_12lags = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,1]-yout[,1])^2)-((x[,1]-yout[,1])^2))
}))
colnames(CSFE_h1_12lags) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/R/algoritmo0.3/CSFE_results/CSFE_h1_12lags.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h1_12lags),  CSFE_h1_12lags, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h1_12lags")

legend("topleft", legend=colnames(CSFE_h1_12lags), col=1:ncol(CSFE_h1_12lags),
       lty=1, cex=0.50, bty="n")

dev.off()

# CSFE h=3 ####
CSFE_h3_12lags = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,3]-yout[,1])^2)-((x[,3]-yout[,1])^2))
}))
colnames(CSFE_h3_12lags) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/R/algoritmo0.3/CSFE_results/CSFE_h3_12lags.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h3_12lags),  CSFE_h3_12lags, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h3_12lags")

legend("topleft", legend=colnames(CSFE_h1_12lags), col=1:ncol(CSFE_h1_12lags),
       lty=1, cex=0.50, bty="n")

dev.off()

# CSFE h=6 ####
CSFE_h6_12lags = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,6]-yout[,1])^2)-((x[,6]-yout[,1])^2))
}))
colnames(CSFE_h6_12lags) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/R/algoritmo0.3/CSFE_results/CSFE_h6_12lags.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h6_12lags),  CSFE_h6_12lags, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h6_12lags")

legend("topleft", legend=colnames(CSFE_h1_12lags), col=1:ncol(CSFE_h1_12lags),
       lty=1, cex=0.50, bty="n")

dev.off()

# CSFE h=12 ####
CSFE_h12_12lags = Reduce(cbind, lapply(models_list, function(x){
  cumsum(((rw[,12]-yout[,1])^2)-((x[,12]-yout[,1])^2))
}))
colnames(CSFE_h12_12lags) = model_files

png("C:/Users/celia/Desktop/ECONOMIA UFRGS/1. INICIAÇÃO CIENTIFICA/R/algoritmo0.3/CSFE_results/CSFE_h12_12lags.png",
    width=1200, height=800, res=150)

matplot(1:nrow(CSFE_h12_12lags),  CSFE_h12_12lags, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h12_12lags")

legend("topleft", legend=colnames(CSFE_h1_12lags), col=1:ncol(CSFE_h1_12lags),
       lty=1, cex=0.50, bty="n")

dev.off()

# salvando ####

save(CSFE_h1_12lags,file = "csfe_results/h1_12lags.rda")
save(CSFE_h3_12lags,file = "csfe_results/h3_12lags.rda")
save(CSFE_h6_12lags,file = "csfe_results/h6_12lags.rda")
save(CSFE_h12_12lags,file = "csfe_results/h12_12lags.rda")


# para visualizar uma curva CSFE ####

load("csfe_results/h1_12lags.rda")
matplot(1:nrow(CSFE_h1_12lags),  CSFE_h1_12lags, type="l", col=1:length(model_files), lty=1,
        xlab="Forecast", ylab="CSFE_h1_12lags")
legend("topleft", legend=colnames(CSFE_h1_12lags), col=1:ncol(CSFE_h1_12lags), lty=1,
       cex=0.50, bty="n")
