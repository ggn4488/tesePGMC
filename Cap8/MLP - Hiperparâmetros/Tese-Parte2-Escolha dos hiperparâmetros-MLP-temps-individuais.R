###########################################
# Rede Neural FF para previsão de cargas
# Configuração dos hiperparâmetros da rede
###########################################
setwd("~/PGMC - 2017-2021/Tese - parte 2/Códigos/MLP - Hiperparâmetros")

##########
# Pacotes
##########
library(erer)
library(fastDummies)
library(ggplot2)
library(keras)

#######################
# Importação dos dados
#######################

data_loads <- read.csv('loadsny1518.csv')
data_temps <- read.csv('tempsny1518.csv')

zone_names <- names(data_loads[,15:25])
temp_names <- names(data_temps)

fd <- 4
ld <- 1461

data_loads$month_sum              <- data_loads$month
data_loads$month_sum[8761:17544]  <- data_loads$month_sum[8761:17544]+12
data_loads$month_sum[17545:26304] <- data_loads$month_sum[17545:26304]+24
data_loads$month_sum[26305:35063] <- data_loads$month_sum[26305:35063]+36

zones_wanted <- c(1,2,4,5,7,9,11)


###################################
# Hiperparâmetro 1 - Learning rate
###################################


for(z in zones_wanted) {

Y         <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),zone_names[z]]
Y24       <- matrix(Y, ncol=24, byrow = T)

month_sum   <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),'month_sum']
month_sum24 <- matrix(month_sum, ncol=24, byrow = T)

# Separação em treino e validação

indices_tr  <- list()
indices_val <- c()

for (i in 1:8) {
  
  indices_tr[[i]]     <-  1:(i*5+i-1)
  indices_val[i]      <-  (i*5)+i
  
}

Y_hoje    <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),zone_names[z]]
Y_hoje24  <- matrix(Y_hoje, ncol=24, byrow = T)

Y_ontem   <- data_loads[which(data_loads$day > (fd-3) & data_loads$day < (ld-3)),zone_names[z]]
Y_ontem24 <- matrix(Y_ontem, ncol=24, byrow = T)

results_lr_list <- time_lr_list <- all_mse_histories_lr_list <- list()

for(t in 1:11) {
  
  all_mse_histories_lr <- list()
  
  temp <- data_temps[,temp_names[t]]
  
  Xt         <- temp[which(data_loads$day > (fd-1) & data_loads$day < (ld-1))]
  Xt24       <- matrix(Xt, ncol=24, byrow = T)
  
  Xt_hoje   <- temp[which(data_loads$day > (fd-2) & data_loads$day < (ld-2))]
  Xt_hoje24 <- matrix(Xt_hoje, ncol=24, byrow = T)
  
  Xt_ontem   <- temp[which(data_loads$day > (fd-3) & data_loads$day < (ld-3))]
  Xt_ontem24 <- matrix(Xt_ontem, ncol=24, byrow = T)
  
  week                <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'weekday']
  week_dummies        <- dummy_cols(week)
  week_dummies        <- week_dummies[,-1]
  names(week_dummies) <- paste('x',as.character(1:7),sep = '')
  
  week_dummies1 <- matrix(week_dummies$x1, ncol=24, byrow = T)
  week_dummies1 <- week_dummies1[,1]
  week_dummies2 <- matrix(week_dummies$x2, ncol=24, byrow = T)
  week_dummies2 <- week_dummies2[,1]
  week_dummies3 <- matrix(week_dummies$x3, ncol=24, byrow = T)
  week_dummies3 <- week_dummies3[,1]
  week_dummies4 <- matrix(week_dummies$x4, ncol=24, byrow = T)
  week_dummies4 <- week_dummies4[,1]
  week_dummies5 <- matrix(week_dummies$x5, ncol=24, byrow = T)
  week_dummies5 <- week_dummies5[,1]
  week_dummies6 <- matrix(week_dummies$x6, ncol=24, byrow = T)
  week_dummies6 <- week_dummies6[,1]
  week_dummies7 <- matrix(week_dummies$x7, ncol=24, byrow = T)
  week_dummies7 <- week_dummies7[,1]
  
  season <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'season']
  season_dummies <- dummy_cols(season)
  season_dummies <- season_dummies[,-1]
  names(season_dummies) <- paste('x',as.character(1:4),sep = '')
  
  season_dummies1 <- matrix(season_dummies$x1, ncol=24, byrow = T)
  season_dummies1 <- season_dummies1[,1]
  season_dummies2 <- matrix(season_dummies$x2, ncol=24, byrow = T)
  season_dummies2 <- season_dummies2[,1]
  season_dummies3 <- matrix(season_dummies$x3, ncol=24, byrow = T)
  season_dummies3 <- season_dummies3[,1]
  season_dummies4 <- matrix(season_dummies$x4, ncol=24, byrow = T)
  season_dummies4 <- season_dummies4[,1]
  
  X24 <- cbind(Xt_ontem24,
               Xt_hoje24,
               Xt24,
               Y_hoje24,
               Y_ontem24,
               week_dummies1,
               week_dummies2,
               week_dummies3,
               week_dummies4,
               week_dummies5,
               week_dummies6,
               week_dummies7,
               season_dummies1,
               season_dummies2,
               season_dummies3,
               season_dummies4)
  
  colnames(X24) <- paste('X',as.character(1:131),sep='')
  
  set.seed(4488)
  
  lr <- runif(5, min = 10e-6, max = 0.1)
  lr <- c(0.01,lr)
  
  set.seed(4488)
  units <- sample(10:31,5)
  units <- c(32,units)
  
  batch <- c(32,64,128,256,1)
  
  epochs <- seq(50,500,50)
  epochs[1] <- 250
  epochs[5] <- 50
  
  optimizer <- c('adam','rmsprop','sgd', 'adamax', 'adagrad')
  
  init <- c('random_uniform', 'random_normal', 'glorot_uniform', 'glorot_normal', 'TruncatedNormal')
  
  build_model <- function() {
    
    model <- keras_model_sequential() %>%
      layer_dense(units = units[1], activation = "relu",
                  input_shape = c(131),kernel_initializer=init[1]) %>%
      layer_dense(units = 24)
    model %>% compile(
      optimizer = optimizer[1],
      loss = "mse",
      metrics = c("mse")
    )
  }
  
  results_lr <- rbind(c(0,0,0,0,0,0),c(t,0,0,0,0,0),c('lr','mediana','média','mínimo','máximo','desvio-padrão'))
  time_lr <- c()
  
  for(l in lr) {
    
    all_mse_lr <- NULL
    
    start_time <- Sys.time()
    
    for(i in 1:8) {
      
      cat('t #', t, 'Z #',z,' Lr #', l, ': processando amostra de validação #', i, '\n')
      
      indices_train <- indices_tr[[i]]
      indice_v      <- indices_val[i]
      
      partial_train_data <- X24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
      val_data           <- X24[which(month_sum24[,1] == indice_v),]
      
      partial_train_targets <- Y24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
      val_targets           <- Y24[which(month_sum24[,1] == indice_v ,),]
      
      mean <- apply(partial_train_data,2,mean)
      sd   <- apply(partial_train_data,2,sd)
      
      partial_train_data <- scale(partial_train_data, center = mean, scale = sd)
      val_data           <- scale(val_data, center = mean, scale = sd)
      
      model <- build_model()
      
      k_set_value(model$optimizer$lr, l)
      
      history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = epochs[1], batch.size = batch[1], verbose = 0
      )
      
      all_mse_lr  <- c(all_mse_lr,history$metrics$val_mse[epochs[1]])
      
      #mape_histories_lr <- c(mape_histories_lr,data.frame(history))
      
    }
    
    results_lr <- rbind(results_lr,c(l,median(all_mse_lr,na.rm=T),mean(all_mse_lr,na.rm=T),min(all_mse_lr,na.rm=T),max(all_mse_lr,na.rm=T),sd(all_mse_lr,na.rm=T)))
    
    all_mse_histories_lr <- c(all_mse_histories_lr,all_mse_lr)
    
    end_time <- Sys.time()
    
    time_lr <- c(time_lr,(end_time - start_time))
    
  }
  
  results_lr_list[[t]] <- results_lr
  time_lr_list[[t]] <- time_lr
  all_mse_histories_lr_list[[t]] <- all_mse_histories_lr
  
}


write.list(z = results_lr_list, file = paste('mlp_lr_',z,'.csv',sep=''))

}

# Resultados

for(z in zones_wanted) {
  
  lr_results <- read.csv(paste('mlp_lr_',zone_names[z],'.csv',sep=''))
  lr <- c()
  
  for(i in 1:11) {
    
    res     <- lr_results[(4+11*(i-1)):(9+11*(i-1)),2:7]
    res[,1] <- as.numeric(as.character(res[,1]))
    res[,2] <- as.numeric(as.character(res[,2]))
    lr      <- rbind(lr,res[which.min(res[,2]),])
    
  }
  
  rownames(lr) <- 1:11
  colnames(lr) <- c('lr','Mediana','Média','Mínimo','Máximo','Desvio-padrão')
  
  write.csv(lr,paste('mlp_lr_results_',zone_names[z],'.csv',sep=''))
  
}

###################################
# Hiperparâmetro 2 - Batch size
###################################
for(z in zones_wanted) {
  
  Y         <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),zone_names[z]]
  Y24       <- matrix(Y, ncol=24, byrow = T)
  
  month_sum   <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),'month_sum']
  month_sum24 <- matrix(month_sum, ncol=24, byrow = T)
  
  # Separação em treino e validação
  
  indices_tr  <- list()
  indices_val <- c()
  
  for (i in 1:8) {
    
    indices_tr[[i]]     <-  1:(i*5+i-1)
    indices_val[i]      <-  (i*5)+i
    
  }
  
  Y_hoje    <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),zone_names[z]]
  Y_hoje24  <- matrix(Y_hoje, ncol=24, byrow = T)
  
  Y_ontem   <- data_loads[which(data_loads$day > (fd-3) & data_loads$day < (ld-3)),zone_names[z]]
  Y_ontem24 <- matrix(Y_ontem, ncol=24, byrow = T)
  
results_bs_list <- time_bs_list <- all_mse_histories_bs_list <- list()

for(t in 1:11) {
  
  all_mse_histories_bs <- list()
  
  temp <- data_temps[,temp_names[t]]
  
  Xt         <- temp[which(data_loads$day > (fd-1) & data_loads$day < (ld-1))]
  Xt24       <- matrix(Xt, ncol=24, byrow = T)
  
  Xt_hoje   <- temp[which(data_loads$day > (fd-2) & data_loads$day < (ld-2))]
  Xt_hoje24 <- matrix(Xt_hoje, ncol=24, byrow = T)
  
  Xt_ontem   <- temp[which(data_loads$day > (fd-3) & data_loads$day < (ld-3))]
  Xt_ontem24 <- matrix(Xt_ontem, ncol=24, byrow = T)
  
  week                <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'weekday']
  week_dummies        <- dummy_cols(week)
  week_dummies        <- week_dummies[,-1]
  names(week_dummies) <- paste('x',as.character(1:7),sep = '')
  
  week_dummies1 <- matrix(week_dummies$x1, ncol=24, byrow = T)
  week_dummies1 <- week_dummies1[,1]
  week_dummies2 <- matrix(week_dummies$x2, ncol=24, byrow = T)
  week_dummies2 <- week_dummies2[,1]
  week_dummies3 <- matrix(week_dummies$x3, ncol=24, byrow = T)
  week_dummies3 <- week_dummies3[,1]
  week_dummies4 <- matrix(week_dummies$x4, ncol=24, byrow = T)
  week_dummies4 <- week_dummies4[,1]
  week_dummies5 <- matrix(week_dummies$x5, ncol=24, byrow = T)
  week_dummies5 <- week_dummies5[,1]
  week_dummies6 <- matrix(week_dummies$x6, ncol=24, byrow = T)
  week_dummies6 <- week_dummies6[,1]
  week_dummies7 <- matrix(week_dummies$x7, ncol=24, byrow = T)
  week_dummies7 <- week_dummies7[,1]
  
  season <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'season']
  season_dummies <- dummy_cols(season)
  season_dummies <- season_dummies[,-1]
  names(season_dummies) <- paste('x',as.character(1:4),sep = '')
  
  season_dummies1 <- matrix(season_dummies$x1, ncol=24, byrow = T)
  season_dummies1 <- season_dummies1[,1]
  season_dummies2 <- matrix(season_dummies$x2, ncol=24, byrow = T)
  season_dummies2 <- season_dummies2[,1]
  season_dummies3 <- matrix(season_dummies$x3, ncol=24, byrow = T)
  season_dummies3 <- season_dummies3[,1]
  season_dummies4 <- matrix(season_dummies$x4, ncol=24, byrow = T)
  season_dummies4 <- season_dummies4[,1]
  
  X24 <- cbind(Xt_ontem24,
               Xt_hoje24,
               Xt24,
               Y_hoje24,
               Y_ontem24,
               week_dummies1,
               week_dummies2,
               week_dummies3,
               week_dummies4,
               week_dummies5,
               week_dummies6,
               week_dummies7,
               season_dummies1,
               season_dummies2,
               season_dummies3,
               season_dummies4)
  
  colnames(X24) <- paste('X',as.character(1:131),sep='')
  
  set.seed(4488)
  
  lr_best <- read.csv(paste('mlp_lr_results_',zone_names[z],'.csv',sep=''))
  lr <- lr_best[t,2]
  
  set.seed(4488)
  
  units <- sample(10:31,5)
  units <- c(32,units)
  
  batch <- c(32,64,128,256,1)
  
  epochs <- seq(50,500,50)
  epochs[1] <- 250
  epochs[5] <- 50
  
  optimizer <- c('adam','rmsprop','sgd', 'adamax', 'adagrad')
  
  init <- c('random_uniform', 'random_normal', 'glorot_uniform', 'glorot_normal', 'TruncatedNormal')
  
  build_model <- function() {
    
    model <- keras_model_sequential() %>%
      layer_dense(units = units[1], activation = "relu",
                  input_shape = c(131),kernel_initializer=init[1]) %>%
      layer_dense(units = 24)
    model %>% compile(
      optimizer = optimizer[1],
      loss = "mse",
      metrics = c("mse")
    )
  }
  
  results_bs <- rbind(c(0,0,0,0,0,0),c(t,0,0,0,0,0),c('bs','mediana','média','mínimo','máximo','desvio-padrão'))
  time_bs <- c()
  
  for(b in batch[-1]) {
    
    all_mse_bs <- NULL
    
    start_time <- Sys.time()
    
    for(i in 1:8) {
      
      cat('t #', t, 'Z #',z,' bs #', b, ': processando amostra de validação #', i, '\n')
      
      indices_train <- indices_tr[[i]]
      indice_v      <- indices_val[i]
      
      partial_train_data <- X24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
      val_data           <- X24[which(month_sum24[,1] == indice_v),]
      
      partial_train_targets <- Y24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
      val_targets           <- Y24[which(month_sum24[,1] == indice_v ,),]
      
      mean <- apply(partial_train_data,2,mean)
      sd   <- apply(partial_train_data,2,sd)
      
      partial_train_data <- scale(partial_train_data, center = mean, scale = sd)
      val_data           <- scale(val_data, center = mean, scale = sd)
      
      model <- build_model()
      
      k_set_value(model$optimizer$lr, lr)
      
      history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = epochs[1], batch.size = batch[b], verbose = 0
      )
      
      all_mse_bs  <- c(all_mse_bs,history$metrics$val_mse[epochs[1]])
      
    }
    
    results_bs <- rbind(results_bs,c(b,median(all_mse_bs,na.rm=T),mean(all_mse_bs,na.rm=T),min(all_mse_bs,na.rm=T),max(all_mse_bs,na.rm=T),sd(all_mse_bs,na.rm=T)))
    
    all_mse_histories_bs <- c(all_mse_histories_bs,all_mse_bs)
    
    end_time <- Sys.time()
    
    time_bs <- c(time_bs,(end_time - start_time))
    
  }
  
  results_bs_list[[t]] <- results_bs
  time_bs_list[[t]] <- time_bs
  all_mse_histories_bs_list[[t]] <- all_mse_histories_bs
  
}

write.list(z = results_bs_list, file = paste('mlp_bs_',zone_names[z],'.csv',sep=''))

}

# Resultados

for(z in zones_wanted) {
  
  bs_results <- read.csv(paste('mlp_bs_',zone_names[z],'.csv',sep=''),stringsAsFactors=FALSE)
  lr         <- read.csv(paste('mlp_lr_results_',zone_names[z],'.csv',sep=''),stringsAsFactors=FALSE)
  bs <- c()
  
  for(i in 1:11) {
    
    res     <- bs_results[(4+9*(i-1)):(7+9*(i-1)),2:7]
    res[,1] <- res[,1]
    res[,2] <- res[,2]
    res[5,] <- c(32,lr$Mediana[i],lr$Média[i],lr$Mínimo[i],lr$Máximo[i],lr$Desvio.padrão[i])
    bs      <- rbind(bs,res[which.min(res[,2]),])
    
  }
  
  rownames(bs) <- 1:11
  colnames(bs) <- c('bs','Mediana','Média','Desvio-padrão','Mínimo','Máximo')
  
  write.csv(bs,paste('mlp_bs_results_',zone_names[z],'.csv',sep=''))
  
}

###################################
# Hiperparâmetro 3 - Épocas
###################################
for(z in zones_wanted) {
  
  Y         <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),zone_names[z]]
  Y24       <- matrix(Y, ncol=24, byrow = T)
  
  month_sum   <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),'month_sum']
  month_sum24 <- matrix(month_sum, ncol=24, byrow = T)
  
  # Separação em treino e validação
  
  indices_tr  <- list()
  indices_val <- c()
  
  for (i in 1:8) {
    
    indices_tr[[i]]     <-  1:(i*5+i-1)
    indices_val[i]      <-  (i*5)+i
    
  }
  
  Y_hoje    <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),zone_names[z]]
  Y_hoje24  <- matrix(Y_hoje, ncol=24, byrow = T)
  
  Y_ontem   <- data_loads[which(data_loads$day > (fd-3) & data_loads$day < (ld-3)),zone_names[z]]
  Y_ontem24 <- matrix(Y_ontem, ncol=24, byrow = T)
  
  results_ep_list <- time_ep_list <- all_mse_histories_ep_list <- list()
  
  for(t in 1:11) {
    
    all_mse_histories_ep <- list()
    
    temp <- data_temps[,temp_names[t]]
    
    Xt         <- temp[which(data_loads$day > (fd-1) & data_loads$day < (ld-1))]
    Xt24       <- matrix(Xt, ncol=24, byrow = T)
    
    Xt_hoje   <- temp[which(data_loads$day > (fd-2) & data_loads$day < (ld-2))]
    Xt_hoje24 <- matrix(Xt_hoje, ncol=24, byrow = T)
    
    Xt_ontem   <- temp[which(data_loads$day > (fd-3) & data_loads$day < (ld-3))]
    Xt_ontem24 <- matrix(Xt_ontem, ncol=24, byrow = T)
    
    week                <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'weekday']
    week_dummies        <- dummy_cols(week)
    week_dummies        <- week_dummies[,-1]
    names(week_dummies) <- paste('x',as.character(1:7),sep = '')
    
    week_dummies1 <- matrix(week_dummies$x1, ncol=24, byrow = T)
    week_dummies1 <- week_dummies1[,1]
    week_dummies2 <- matrix(week_dummies$x2, ncol=24, byrow = T)
    week_dummies2 <- week_dummies2[,1]
    week_dummies3 <- matrix(week_dummies$x3, ncol=24, byrow = T)
    week_dummies3 <- week_dummies3[,1]
    week_dummies4 <- matrix(week_dummies$x4, ncol=24, byrow = T)
    week_dummies4 <- week_dummies4[,1]
    week_dummies5 <- matrix(week_dummies$x5, ncol=24, byrow = T)
    week_dummies5 <- week_dummies5[,1]
    week_dummies6 <- matrix(week_dummies$x6, ncol=24, byrow = T)
    week_dummies6 <- week_dummies6[,1]
    week_dummies7 <- matrix(week_dummies$x7, ncol=24, byrow = T)
    week_dummies7 <- week_dummies7[,1]
    
    season <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'season']
    season_dummies <- dummy_cols(season)
    season_dummies <- season_dummies[,-1]
    names(season_dummies) <- paste('x',as.character(1:4),sep = '')
    
    season_dummies1 <- matrix(season_dummies$x1, ncol=24, byrow = T)
    season_dummies1 <- season_dummies1[,1]
    season_dummies2 <- matrix(season_dummies$x2, ncol=24, byrow = T)
    season_dummies2 <- season_dummies2[,1]
    season_dummies3 <- matrix(season_dummies$x3, ncol=24, byrow = T)
    season_dummies3 <- season_dummies3[,1]
    season_dummies4 <- matrix(season_dummies$x4, ncol=24, byrow = T)
    season_dummies4 <- season_dummies4[,1]
    
    X24 <- cbind(Xt_ontem24,
                 Xt_hoje24,
                 Xt24,
                 Y_hoje24,
                 Y_ontem24,
                 week_dummies1,
                 week_dummies2,
                 week_dummies3,
                 week_dummies4,
                 week_dummies5,
                 week_dummies6,
                 week_dummies7,
                 season_dummies1,
                 season_dummies2,
                 season_dummies3,
                 season_dummies4)
    
    colnames(X24) <- paste('X',as.character(1:131),sep='')
    
    set.seed(4488)
    
    lr_best <- read.csv(paste('mlp_lr_results_',zone_names[z],'.csv',sep=''))
    lr <- lr_best[t,2]
    
    bs_best <- read.csv(paste('mlp_bs_results_',zone_names[z],'.csv',sep=''))
    bs <- bs_best[t,2]
    
    set.seed(4488)
    
    units <- sample(10:31,5)
    units <- c(32,units)
    
    epochs <- seq(50,500,50)
    epochs[1] <- 250
    epochs[5] <- 50
    
    optimizer <- c('adam','rmsprop','sgd', 'adamax', 'adagrad')
    
    init <- c('random_uniform', 'random_normal', 'glorot_uniform', 'glorot_normal', 'TruncatedNormal')
    
    build_model <- function() {
      
      model <- keras_model_sequential() %>%
        layer_dense(units = units[1], activation = "relu",
                    input_shape = c(131),kernel_initializer=init[1]) %>%
        layer_dense(units = 24)
      model %>% compile(
        optimizer = optimizer[1],
        loss = "mse",
        metrics = c("mse")
      )
    }
    
    results_ep <- rbind(c(0,0,0,0,0,0),c(t,0,0,0,0,0),c('ep','mediana','média','mínimo','máximo','desvio-padrão'))
    time_ep <- c()
    
    for(ep in epochs[-1]) {
      
      all_mse_ep <- NULL
      
      start_time <- Sys.time()
      
      for(i in 1:8) {
        
        cat('t #', t, 'Z #',z,' ep #', ep, ': processando amostra de validação #', i, '\n')
        
        indices_train <- indices_tr[[i]]
        indice_v      <- indices_val[i]
        
        partial_train_data <- X24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
        val_data           <- X24[which(month_sum24[,1] == indice_v),]
        
        partial_train_targets <- Y24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
        val_targets           <- Y24[which(month_sum24[,1] == indice_v ,),]
        
        mean <- apply(partial_train_data,2,mean)
        sd   <- apply(partial_train_data,2,sd)
        
        partial_train_data <- scale(partial_train_data, center = mean, scale = sd)
        val_data           <- scale(val_data, center = mean, scale = sd)
        
        model <- build_model()
        
        k_set_value(model$optimizer$lr, lr)
        
        history <- model %>% fit(
          partial_train_data, partial_train_targets,
          validation_data = list(val_data, val_targets),
          epochs = ep, batch.size = bs, verbose = 0
        )
        
        all_mse_ep  <- c(all_mse_ep,history$metrics$val_mse[ep])
        
      }
      
      results_ep <- rbind(results_ep,c(ep,median(all_mse_ep,na.rm=T),mean(all_mse_ep,na.rm=T),min(all_mse_ep,na.rm=T),max(all_mse_ep,na.rm=T),sd(all_mse_ep,na.rm=T)))
      
      all_mse_histories_ep <- c(all_mse_histories_ep,all_mse_ep)
      
      end_time <- Sys.time()
      
      time_ep <- c(time_ep,(end_time - start_time))
      
    }
    
    results_ep_list[[t]] <- results_ep
    time_ep_list[[t]] <- time_ep
    all_mse_histories_ep_list[[t]] <- all_mse_histories_ep
    
  }
  
  write.list(z = results_ep_list, file = paste('mlp_ep_',zone_names[z],'.csv',sep=''))
  
}

for(z in zones_wanted) {
  
  ep_results <- read.csv(paste('mlp_ep_',zone_names[z],'.csv',sep=''),stringsAsFactors=FALSE)
  bs         <- read.csv(paste('mlp_bs_results_',zone_names[z],'.csv',sep=''),stringsAsFactors=FALSE)
  ep <- c()
  
  for(i in 1:11) {
    
    res     <- ep_results[(4+14*(i-1)):(12+14*(i-1)),2:7]
    res[,1] <- res[,1]
    res[,2] <- res[,2]
    res[5,] <- c(250,bs$Mediana[i],bs$Média[i],bs$Mínimo[i],bs$Máximo[i],bs$Desvio.padrão[i])
    ep      <- rbind(ep,res[which.min(res[,2]),])
    
  }
  
  rownames(ep) <- 1:11
  colnames(ep) <- c('ep','Mediana','Média','Desvio-padrão','Mínimo','Máximo')
  
  write.csv(ep,paste('mlp_ep_results_',zone_names[z],'.csv',sep=''))
  
}

###################################
# Hiperparâmetro 4 - Hidden units
###################################
for(z in zones_wanted[7]) {
  
  Y         <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),zone_names[z]]
  Y24       <- matrix(Y, ncol=24, byrow = T)
  
  month_sum   <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),'month_sum']
  month_sum24 <- matrix(month_sum, ncol=24, byrow = T)
  
  # Separação em treino e validação
  
  indices_tr  <- list()
  indices_val <- c()
  
  for (i in 1:8) {
    
    indices_tr[[i]]     <-  1:(i*5+i-1)
    indices_val[i]      <-  (i*5)+i
    
  }
  
  Y_hoje    <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),zone_names[z]]
  Y_hoje24  <- matrix(Y_hoje, ncol=24, byrow = T)
  
  Y_ontem   <- data_loads[which(data_loads$day > (fd-3) & data_loads$day < (ld-3)),zone_names[z]]
  Y_ontem24 <- matrix(Y_ontem, ncol=24, byrow = T)
  
  results_hu_list <- time_hu_list <- all_mse_histories_hu_list <- list()
  
  for(t in 1:11) {
    
    all_mse_histories_hu <- list()
    
    temp <- data_temps[,temp_names[t]]
    
    Xt         <- temp[which(data_loads$day > (fd-1) & data_loads$day < (ld-1))]
    Xt24       <- matrix(Xt, ncol=24, byrow = T)
    
    Xt_hoje   <- temp[which(data_loads$day > (fd-2) & data_loads$day < (ld-2))]
    Xt_hoje24 <- matrix(Xt_hoje, ncol=24, byrow = T)
    
    Xt_ontem   <- temp[which(data_loads$day > (fd-3) & data_loads$day < (ld-3))]
    Xt_ontem24 <- matrix(Xt_ontem, ncol=24, byrow = T)
    
    week                <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'weekday']
    week_dummies        <- dummy_cols(week)
    week_dummies        <- week_dummies[,-1]
    names(week_dummies) <- paste('x',as.character(1:7),sep = '')
    
    week_dummies1 <- matrix(week_dummies$x1, ncol=24, byrow = T)
    week_dummies1 <- week_dummies1[,1]
    week_dummies2 <- matrix(week_dummies$x2, ncol=24, byrow = T)
    week_dummies2 <- week_dummies2[,1]
    week_dummies3 <- matrix(week_dummies$x3, ncol=24, byrow = T)
    week_dummies3 <- week_dummies3[,1]
    week_dummies4 <- matrix(week_dummies$x4, ncol=24, byrow = T)
    week_dummies4 <- week_dummies4[,1]
    week_dummies5 <- matrix(week_dummies$x5, ncol=24, byrow = T)
    week_dummies5 <- week_dummies5[,1]
    week_dummies6 <- matrix(week_dummies$x6, ncol=24, byrow = T)
    week_dummies6 <- week_dummies6[,1]
    week_dummies7 <- matrix(week_dummies$x7, ncol=24, byrow = T)
    week_dummies7 <- week_dummies7[,1]
    
    season <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),'season']
    season_dummies <- dummy_cols(season)
    season_dummies <- season_dummies[,-1]
    names(season_dummies) <- paste('x',as.character(1:4),sep = '')
    
    season_dummies1 <- matrix(season_dummies$x1, ncol=24, byrow = T)
    season_dummies1 <- season_dummies1[,1]
    season_dummies2 <- matrix(season_dummies$x2, ncol=24, byrow = T)
    season_dummies2 <- season_dummies2[,1]
    season_dummies3 <- matrix(season_dummies$x3, ncol=24, byrow = T)
    season_dummies3 <- season_dummies3[,1]
    season_dummies4 <- matrix(season_dummies$x4, ncol=24, byrow = T)
    season_dummies4 <- season_dummies4[,1]
    
    X24 <- cbind(Xt_ontem24,
                 Xt_hoje24,
                 Xt24,
                 Y_hoje24,
                 Y_ontem24,
                 week_dummies1,
                 week_dummies2,
                 week_dummies3,
                 week_dummies4,
                 week_dummies5,
                 week_dummies6,
                 week_dummies7,
                 season_dummies1,
                 season_dummies2,
                 season_dummies3,
                 season_dummies4)
    
    colnames(X24) <- paste('X',as.character(1:131),sep='')
    
    set.seed(4488)
    
    lr_best <- read.csv(paste('mlp_lr_results_',zone_names[z],'.csv',sep=''))
    lr <- lr_best[t,2]
    
    bs_best <- read.csv(paste('mlp_bs_results_',zone_names[z],'.csv',sep=''))
    bs <- bs_best[t,2]
    
    ep_best <- read.csv(paste('mlp_ep_results_',zone_names[z],'.csv',sep=''))
    ep <- ep_best[t,2]
    
    set.seed(4488)
    
    units <- sample(10:31,5)
    units <- c(32,units)
    
    optimizer <- c('adam','rmsprop','sgd', 'adamax', 'adagrad')
    
    init <- c('random_uniform', 'random_normal', 'glorot_uniform', 'glorot_normal', 'TruncatedNormal')
    
    results_hu <- rbind(c(0,0,0,0,0,0),c(t,0,0,0,0,0),c('hu','mediana','média','mínimo','máximo','desvio-padrão'))
    time_hu <- c()
    
    for(hu in units[-1]) {
      
      all_mse_hu <- NULL
      
      start_time <- Sys.time()
      
      for(i in 1:8) {
        
        cat('t #', t, 'Z #',z,' hu #', hu, ': processando amostra de validação #', i, '\n')
        
        indices_train <- indices_tr[[i]]
        indice_v      <- indices_val[i]
        
        partial_train_data <- X24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
        val_data           <- X24[which(month_sum24[,1] == indice_v),]
        
        partial_train_targets <- Y24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
        val_targets           <- Y24[which(month_sum24[,1] == indice_v ,),]
        
        mean <- apply(partial_train_data,2,mean)
        sd   <- apply(partial_train_data,2,sd)
        
        partial_train_data <- scale(partial_train_data, center = mean, scale = sd)
        val_data           <- scale(val_data, center = mean, scale = sd)
    
        build_model <- function() {
          
          model <- keras_model_sequential() %>%
            layer_dense(units = hu, activation = "relu",
                        input_shape = c(131),kernel_initializer=init[1]) %>%
            layer_dense(units = 24)
          model %>% compile(
            optimizer = optimizer[1],
            loss = "mse",
            metrics = c("mse")
          )
        }
        
        model <- build_model()
        
        k_set_value(model$optimizer$lr, lr)
        
        history <- model %>% fit(
          partial_train_data, partial_train_targets,
          validation_data = list(val_data, val_targets),
          epochs = ep, batch.size = bs, verbose = 0
        )
        
        all_mse_hu  <- c(all_mse_hu,history$metrics$val_mse[ep])
        
      }
      
      results_hu <- rbind(results_hu,c(hu,median(all_mse_hu,na.rm=T),mean(all_mse_hu,na.rm=T),min(all_mse_hu,na.rm=T),max(all_mse_hu,na.rm=T),sd(all_mse_hu,na.rm=T)))
      
      all_mse_histories_hu <- c(all_mse_histories_hu,all_mse_hu)
      
      end_time <- Sys.time()
      
      time_hu <- c(time_hu,(end_time - start_time))
      
    }
    
    results_hu_list[[t]] <- results_hu
    time_hu_list[[t]] <- time_hu
    all_mse_histories_hu_list[[t]] <- all_mse_histories_hu
    
  }
  
  write.list(z = results_hu_list, file = paste('mlp_hu_',zone_names[z],'.csv',sep=''))
  
}

for(z in zones_wanted) {
  
  hu_results <- read.csv(paste('mlp_hu_',zone_names[z],'.csv',sep=''),stringsAsFactors=FALSE)
  ep         <- read.csv(paste('mlp_ep_results_',zone_names[z],'.csv',sep=''),stringsAsFactors=FALSE)
  hu <- c()
  
  for(i in 1:11) {
    
    res     <- hu_results[(4+10*(i-1)):(8+10*(i-1)),2:7]
    res[,1] <- res[,1]
    res[,2] <- res[,2]
    res[5,] <- c(32,ep$Mediana[i],ep$Média[i],ep$Mínimo[i],ep$Máximo[i],ep$Desvio.padrão[i])
    hu      <- rbind(hu,res[which.min(res[,2]),])
    
  }
  
  rownames(hu) <- 1:11
  colnames(hu) <- c('hu','Mediana','Média','Desvio-padrão','Mínimo','Máximo')
  
  write.csv(hu,paste('mlp_hu_results_',zone_names[z],'.csv',sep=''))
  
}