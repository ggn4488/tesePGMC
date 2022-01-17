#################################
# Modelos de séries temporais
#################################

library(MLmetrics)
library(smooth)
library(forecast)
library(reshape2)

######################
# Preparação dos dados
######################

# Diretório dos dados
setwd("~/PGMC - 2017-2021/Tese/Códigos/Dados")

# Importa os dados
loads <- read.csv('loadsny1518.csv',header=TRUE,sep=",")

tr    <- loads[which((loads$day >= 4) & (loads$day <= 1102)),]           # Dados de treino
a_tr  <- array(data = NA, dim = c((tr$day[nrow(tr)]-tr$day[1]+1),24,11)) # Dados de treino separados por hora do dia
te    <- loads[which((loads$day >= 1103) & (loads$day <= 1459)),]        # Dados de teste
a_te  <- array(data = NA, dim = c((te$day[nrow(te)]-te$day[1]+1),24,11)) # Dados de teste separados por hora do dia

# Constrói os arrays com os dados separados por hora do dia
for(j in 1:11) {
  for (k in 1:24) {
  a_tr[,k,j] <- tr[which(tr$hour==k),14+j]
  a_te[,k,j] <- te[which(te$hour==k),14+j]
  }
}

# Nomes das zonas
zones <- colnames(tr[,15:25])

########
# Naive
########

# Inicializa matrizes para métricas de erro
MAPE_te  <- MSE_te  <- RMSE_te <- matrix(nrow=11,ncol=2)

APEs_te_naive24 <- matrix(data = NA, nrow = nrow(te)-24, ncol = 11)
APEs_te_naive168 <- matrix(data = NA, nrow = nrow(te)-168, ncol = 11)

rownames(MAPE_te) <- rownames(MSE_te) <- rownames(RMSE_te) <- colnames(APEs_te_naive24) <- colnames(APEs_te_naive168) <- zones
colnames(MAPE_te) <- colnames(MSE_te) <- colnames(RMSE_te) <- c('Naive24','Naive168')

for(j in zones) {
  
  y             <- te[[j]]
  
  MAPE_te[j,'Naive24']   <- MAPE(y[1:(length(y)-24)],y[25:length(y)])*100  
  MAPE_te[j,'Naive168']  <- MAPE(y[1:(length(y)-168)],y[169:length(y)])*100

  APEs_te_naive24[,j]  <- abs((y[1:(length(y)-24)]-y[25:length(y)])/y[25:length(y)])*100
  APEs_te_naive168[,j] <- abs((y[1:(length(y)-168)]-y[169:length(y)])/y[169:length(y)])*100
  
  MSE_te[j,'Naive24']   <- MSE(y[1:(length(y)-24)],y[25:length(y)])    
  MSE_te[j,'Naive168']  <- MSE(y[1:(length(y)-168)],y[169:length(y)])    
  
  RMSE_te[j,'Naive24']   <- RMSE(y[1:(length(y)-24)],y[25:length(y)])
  RMSE_te[j,'Naive168']  <- RMSE(y[1:(length(y)-168)],y[169:length(y)])   
  
}

APEs_te_naive24  <- data.frame(te$hour[25:nrow(te)], APEs_te_naive24)
APEs_te_naive168 <- data.frame(te$hour[169:nrow(te)], APEs_te_naive168)

colnames(APEs_te_naive24) <- colnames(APEs_te_naive168) <- c('hour',zones)

MAPEs_te_naive24_h  <- MAPEs_te_naive168_h <- matrix(data = NA, nrow = 11, ncol=24)

for (k in 1:11) {
  for (j in 1:24) {
  MAPEs_te_naive24_h[k,j]  <- mean(APEs_te_naive24[which(APEs_te_naive24$hour==j),k+1])
  MAPEs_te_naive168_h[k,j] <- mean(APEs_te_naive168[which(APEs_te_naive168$hour==j),k+1])
  }
}

#############################################
# 3.2 Holt-Winters-Taylor
#############################################
MAPE_te <- cbind(MAPE_te,matrix(data=NA,nrow=nrow(MAPE_te),ncol=1))

MSE_te <- cbind(MSE_te,matrix(data=NA,nrow=nrow(MSE_te),ncol=1))

RMSE_te <- cbind(RMSE_te,matrix(data=NA,nrow=nrow(RMSE_te),ncol=1))

colnames(MAPE_te)[3] <- colnames(MSE_te)[3] <- colnames(RMSE_te)[3] <- 'HWT'

hwtopt <- function(data,const) 
{
  Y      <- data
  n      <- length(Y)
  Yprev  <- L <- d <- w <- matrix(0,n,1)
  s1     <- 24
  s2     <- 168
  phi    <- const[1]
  lambda <- const[2]
  delta  <- const[3]
  omega  <- const[4]
  
  # Inicialização do nível (média das cargas da primeira semana)
  media <- mean(Y[1:168])
  niv   <- rep(media,168)
  L[1:168,1] <- niv
  
  # Inicialização dos fatores diários
  # media das observacoes nos dias da semana menos o nível L
  seg   <- Y[01:24]
  ter   <- Y[25:48]
  qua   <- Y[49:72]
  qui   <- Y[73:96]
  sex   <- Y[97:120]
  media <- (seg+ter+qua+qui+sex)/5
  fat   <- rep(media,7)-niv
  d[1:168,1] <- fat
  rm(niv,fat,seg,ter,qua,qui,sex,media)
  
  # Inicialização dos fatores semanais
  # valor das observações menos o nivel e os fatores diarios 
  fat <- Y[1:168]-L[1:168]-d[1:168]  
  w[1:168,1] <- fat
  rm(fat)
  
  # Previsão 
  for (t in 168:(n-1))
  {
    # Atualização
    if (t > 168) 
    {	 
      L[t,1] <- lambda*(Y[t,1]-d[t-s1,1]-w[t-s2,1]) + (1-lambda)*L[t-1 ,1]
      d[t,1] <-  delta*(Y[t,1]-L[t,1]-w[t-s2,1]) + (1-delta) *d[t-s1,1]
      w[t,1] <-  omega*(Y[t,1]-L[t,1]-d[t,1]) + (1-omega)*w[t-s2,1]
    }
    # Previsão
    if (t%%24 == 0) 
    {
      for (k in 1:24) 
      {
        if (t == 168)  Yprev[t+k,1] <- L[t,1]+d[t-s1+k,1]+w[t-s2+k,1] 
        else           Yprev[t+k,1] <- L[t,1]+d[t-s1+k,1]+w[t-s2+k,1] +    
            (phi^k)*(Y[t,1]-(L[t-1,1]+d[t-s1,1]+w[t-s2,1])); 
      }
    }  
  }
  
  erro <- MSE(Yprev[,1],Y[,1])
  return(erro)
}

hwt = function(Y,s1,s2,lambda,delta,omega,phi) 
{
  n = length(Y)
  Yprev = L = d = w = matrix(0,n,1)
  
  # Inicialização do nível (média das cargas da primeira semana)
  media= mean(Y[1:168])
  niv= rep(media,168)
  L[1:168,1]= niv
  
  # Inicialização dos fatores diários
  # media das observacoes nos dias da semana menos o nível L
  seg= Y[01: 24]
  ter= Y[25: 48]
  qua= Y[49: 72]
  qui= Y[73: 96]
  sex= Y[97:120]
  media= (seg+ter+qua+qui+sex)/5
  fat= rep(media,7)-niv
  d[1:168,1]= fat
  rm(niv,fat,seg,ter,qua,qui,sex,media)
  
  # Inicialização dos fatores semanais
  # valor das observações menos o nivel e os fatores diarios 
  fat= Y[1:168]-L[1:168]-d[1:168]  
  w[1:168,1]= fat
  rm(fat)
  
  # Previsão
  for (t in 168:(n-1))
  {
    # Atualização
    if (t > 168) 
    { 
      L[t,1]=lambda*(Y[t,1]-d[t-s1,1]-w[t-s2,1])+(1-lambda)*L[t-1 ,1]
      d[t,1]= delta*(Y[t,1]-L[t,1]-w[t-s2,1])+(1-delta)* d[t-s1,1]
      w[t,1]= omega*(Y[t,1]-L[t,1]-d[t,1])+(1-omega)* w[t-s2,1]
    }
    # Previsao
    if (t%%24 == 0) 
    {
      for (k in 1:24) 
      {
        if (t == 168) Yprev[t+k,1] = L[t,1]+d[t-s1+k,1]+w[t-s2+k,1] 
        else Yprev[t+k,1] = L[t,1]+d[t-s1+k,1]+w[t-s2+k,1] +(phi^k)*(Y[t,1]-(L[t-1,1]+d[t-s1,1]+w[t-s2,1])); 
      }
    }
  }
  saida = cbind(Yprev,L,d,w)	
  return(saida)  
}

const_hwt           <- matrix(data=NA,nrow=11,ncol=4)
rownames(const_hwt) <- zones
colnames(const_hwt) <- c('phi','lambda','delta','omega')

APEs_te_hwt <- matrix(data = NA, nrow = nrow(te)-168, ncol = 11)
colnames(APEs_te_hwt) <- zones

time_hwt <- list()

for(j in zones) {
start_time <- Sys.time()
ytr             <- tr[[j]]
yte             <- te[[j]]

set.seed(4488)

result_hwt <- optim(runif(4,min=0,max=1), 
                     hwtopt, 
                     data=as.matrix(ytr), 
                     method="L-BFGS-B",
                     lower =  c(0,0,0,0), 
                     upper = c(1,1,1,1))

end_time <- Sys.time()
time_hwt <- c(time_hwt,(end_time - start_time))

s1  		<- 24
s2  		<- 168
phi    	<- const_hwt[j,'phi']     <- result_hwt$par[1]    
lambda 	<- const_hwt[j,'lambda']  <- result_hwt$par[2] 
delta  	<- const_hwt[j,'delta']   <- result_hwt$par[3] 
omega  	<- const_hwt[j,'omega']   <- result_hwt$par[4]

prev_hwt_tr <- hwt(as.matrix(ytr),s1,s2,lambda,delta,omega,phi)
prev_hwt_te <- hwt(as.matrix(yte),s1,s2,lambda,delta,omega,phi)
write.csv(prev_hwt_tr,file=paste('prev_hwt_tr', j,'.csv', sep=''))
write.csv(prev_hwt_te,file=paste('prev_hwt_te', j,'.csv', sep=''))

MAPE_te[j,'HWT'] <- MAPE(prev_hwt_te[169:nrow(prev_hwt_te),1],yte[169:length(yte)])*100

APEs_te_hwt[,j] <- abs((prev_hwt_te[169:nrow(prev_hwt_te),1]-yte[169:length(yte)])/yte[169:length(yte)])*100

MSE_te[j,'HWT']  <- MSE(prev_hwt_te[169:nrow(prev_hwt_te),1],yte[169:length(yte)])
RMSE_te[j,'HWT'] <- RMSE(prev_hwt_te[169:nrow(prev_hwt_te),1],yte[169:length(yte)])

}

APEs_te_hwt <- data.frame(te$hour[169:nrow(te)], APEs_te_hwt)

colnames(APEs_te_hwt) <- c('hour',zones)

MAPEs_te_hwt  <- matrix(data = NA, nrow = 11, ncol=24)

for (k in 1:11) {
  for (j in 1:24) {
    MAPEs_te_hwt[k,j] <- mean(APEs_te_hwt[which(APEs_te_hwt$hour==j),k+1])
  }
}


#############
# 3.3 Sarima
#############
MAPE_te_sarima    <- MSE_te_sarima <- RMSE_te_sarima <- matrix(data = NA, nrow = 11, ncol = 24)
a_APEs_te_sarima  <- array(data = NA, dim = c((te$day[nrow(te)]-te$day[1]+1),24,11))
coefs_sarima      <- list()
time_sarima       <- list()

for (j in 1:11) {
  for (k in 1:24) {
    start_time <- Sys.time()
    set.seed(4488)
    sarima_tr               <- auto.arima(ts(a_tr[,k,j],frequency = 7))
    
    end_time <- Sys.time()
    time_sarima <- c(time_sarima,(end_time - start_time))
    
    coefs_sarima            <- list(coefs_sarima,sarima_tr$coef)
    prev_sarima             <- Arima(ts(a_te[,k,j],frequency = 7), model=sarima_tr)
    prev_sarima             <- fitted(prev_sarima)
    
    
    MAPE_te_sarima[j,k]     <- MAPE(a_te[,k,j],prev_sarima)*100
    a_APEs_te_sarima[,k,j]  <- abs((prev_sarima-a_te[,k,j])/a_te[,k,j])*100
    
    MSE_te_sarima[j,k]      <- MAPE(a_te[,k,j],prev_sarima)*100
    RMSE_te_sarima[j,k]     <- MAPE(a_te[,k,j],prev_sarima)*100
  }
}

APEs_te_sarima <- matrix(data = NA, nrow = nrow(te), ncol = 11)
MAPEs_te_sarima <- matrix(data = NA, nrow = 11, ncol = 1)

for(j in 1:11) {
  APEs_te_sarima[,j] <- as.vector(a_APEs_te_sarima[,,j])
  MAPEs_te_sarima[j] <- mean(MAPE_te_sarima[j,])
}

MAPE_te <- cbind(MAPE_te,MAPEs_te_sarima)
colnames(MAPE_te)[4] <- 'Sarima'

# library(tseries)
# adf.test(quandldata[,1],alternative = 'stationary'
# resíduos