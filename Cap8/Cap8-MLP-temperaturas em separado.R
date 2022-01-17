setwd("~/PGMC - 2017-2021/Tese - parte 2/Códigos/MLP - Hiperparâmetros")

library(fastDummies)
library(ggplot2)
library(keras)

data_loads <- read.csv('loadsny1518.csv')
data_temps <- read.csv('tempsny1518.csv')

zone_names <- names(data_loads[,15:25])
temp_names <- names(data_temps)

#z<-'NORTH'
#data_loads[19226,z] <- (data_loads[19225,z] + data_loads[19227,z])/2


zones_wanted <- 9

for(z in zones_wanted) {

setwd("~/PGMC - 2017-2021/Tese - parte 2/Códigos/MLP - Hiperparâmetros")
  
lr_final <- read.csv(paste('mlp_lr_results_',zone_names[z],'.csv',sep=''))
bs_final <- read.csv(paste('mlp_bs_results_',zone_names[z],'.csv',sep=''))
ep_final <- read.csv(paste('mlp_ep_results_',zone_names[z],'.csv',sep=''))
hu_final <- read.csv(paste('mlp_hu_results_',zone_names[z],'.csv',sep=''))

setwd(paste("~/PGMC - 2017-2021/Tese - parte 2/Códigos/MLP - Temperaturas em separado/",zone_names[z],sep=''))

fd <- 4
ld <- 1461

data_loads$month_sum              <- data_loads$month
data_loads$month_sum[8761:17544]  <- data_loads$month_sum[8761:17544]+12
data_loads$month_sum[17545:26304] <- data_loads$month_sum[17545:26304]+24
data_loads$month_sum[26305:35063] <- data_loads$month_sum[26305:35063]+36

Y         <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),zone_names[z]]
Y24       <- matrix(Y, ncol=24, byrow = T)

month_sum   <- data_loads[which(data_loads$day > (fd-1) & data_loads$day < (ld-1)),'month_sum']
month_sum24 <- matrix(month_sum, ncol=24, byrow = T)

Y_hoje    <- data_loads[which(data_loads$day > (fd-2) & data_loads$day < (ld-2)),zone_names[z]]
Y_hoje24  <- matrix(Y_hoje, ncol=24, byrow = T)

Y_ontem   <- data_loads[which(data_loads$day > (fd-3) & data_loads$day < (ld-3)),zone_names[z]]
Y_ontem24 <- matrix(Y_ontem, ncol=24, byrow = T)

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

indices_train <- 1:36
indice_test      <- 37:48

predicts_train <-  vector(mode = "list", 11)
predicts <- predicts_r1 <- predicts_r2 <- predicts_r3 <- predicts_r4 <- predicts_r5 <- vector(mode = "list", 11)


for(t in 1:11) {

lr_final_t <- as.numeric(lr_final[t,2])  
bs_final_t <- as.numeric(bs_final[t,2])
ep_final_t <- as.numeric(ep_final[t,2])
hu_final_t <- as.numeric(hu_final[t,2])

predicts_train[[t]] <- vector(mode = "list", 100)  
predicts[[t]] <- vector(mode = "list", 100)

  
temp <- data_temps[,temp_names[t]]

Xt         <- temp[which(data_loads$day > (fd-1) & data_loads$day < (ld-1))]
Xt24       <- matrix(Xt, ncol=24, byrow = T)

Xt_hoje   <- temp[which(data_loads$day > (fd-2) & data_loads$day < (ld-2))]
Xt_hoje24 <- matrix(Xt_hoje, ncol=24, byrow = T)

Xt_ontem   <- temp[which(data_loads$day > (fd-3) & data_loads$day < (ld-3))]
Xt_ontem24 <- matrix(Xt_ontem, ncol=24, byrow = T)

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

train_data <- X24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
test_data           <- X24[which(month_sum24[,1] >= indice_test[1]),]

train_targets <- Y24[which(month_sum24[,1] <= indices_train[length(indices_train)]),]
test_targets           <- Y24[which(month_sum24[,1] >= indice_test[1] ,),]

mean <- apply(train_data,2,mean)
sd   <- apply(train_data,2,sd)

train_data <- scale(train_data, center = mean, scale = sd)
test_data           <- scale(test_data, center = mean, scale = sd)

test_data_r0 <- test_data_r1 <- test_data_r2 <- test_data_r3 <- test_data_r4 <- test_data_r5 <-
  X24[which(month_sum24[,1] >= indice_test[1]),]

for(j in 1:120) {
  set.seed(4488) 
  r1 <- rnorm(nrow(test_data_r0),mean=0,sd=0.01*mean(test_data_r0[,j]))
  set.seed(4488)
  r2 <- rnorm(nrow(test_data_r0),mean=0,sd=0.02*mean(test_data_r0[,j]))  
  set.seed(4488)
  r3 <- rnorm(nrow(test_data_r0),mean=0,sd=0.03*mean(test_data_r0[,j]))  
  set.seed(4488)
  r4 <- rnorm(nrow(test_data_r0),mean=0,sd=0.04*mean(test_data_r0[,j]))  
  set.seed(4488)
  r5 <- rnorm(nrow(test_data_r0),mean=0,sd=0.05*mean(test_data_r0[,j]))
  
  test_data_r1[,j] <- test_data_r1[,j] + r1
  test_data_r2[,j] <- test_data_r2[,j] + r2
  test_data_r3[,j] <- test_data_r3[,j] + r3
  test_data_r4[,j] <- test_data_r4[,j] + r4
  test_data_r5[,j] <- test_data_r5[,j] + r5

}

test_data_r1           <- scale(test_data_r1, center = mean, scale = sd)
test_data_r2           <- scale(test_data_r2, center = mean, scale = sd)
test_data_r3           <- scale(test_data_r3, center = mean, scale = sd)
test_data_r4           <- scale(test_data_r4, center = mean, scale = sd)
test_data_r5           <- scale(test_data_r5, center = mean, scale = sd)

build_model <- function() {
  
  model <- keras_model_sequential() %>%
    layer_dense(units = hu_final_t, activation = "relu",
                input_shape = c(131),kernel_initializer='random_uniform',
                ) %>%
    layer_dense(units = 24)
  model %>% compile(
    optimizer = 'adam',
    loss = "mse",
    metrics = c("mape")
  )
}

time <- c()
results <- results_r1 <- results_r2 <- results_r3 <- results_r4 <- results_r5 <- list()

for(i in 1:100) {
  
  cat("Temp ", t, "it ", i, 'zone', z, "\n")
  
  start_time <- Sys.time()
    
  model <- build_model()
  
  k_set_value(model$optimizer$lr, lr_final_t)
      
  history <- model %>% fit(
      train_data, train_targets,
      epochs = ep_final_t, batch.size = bs_final_t, verbose=0,
      view_metrics = FALSE
    )
  
  predicts_train[[t]][[i]]    <- model %>% predict(train_data)
  
  predicts[[t]][[i]]    <- model %>% predict(test_data)
  predicts_r1[[t]][[i]] <- model %>% predict(test_data)
  predicts_r2[[t]][[i]] <- model %>% predict(test_data)
  predicts_r3[[t]][[i]] <- model %>% predict(test_data)
  predicts_r4[[t]][[i]] <- model %>% predict(test_data)
  predicts_r5[[t]][[i]] <- model %>% predict(test_data)
  
  result    <- model %>% evaluate(test_data,test_targets)
  result_r1 <- model %>% evaluate(test_data_r1,test_targets)
  result_r2 <- model %>% evaluate(test_data_r2,test_targets)
  result_r3 <- model %>% evaluate(test_data_r3,test_targets)
  result_r4 <- model %>% evaluate(test_data_r4,test_targets)
  result_r5 <- model %>% evaluate(test_data_r5,test_targets)
  
  results[[i]]    <- result
  results_r1[[i]] <- result_r1
  results_r2[[i]] <- result_r2
  results_r3[[i]] <- result_r3
  results_r4[[i]] <- result_r4
  results_r5[[i]] <- result_r5
  
  end_time <- Sys.time()
    
  time <- c(time,(end_time - start_time))
  
}

mape <- mape_r1 <- mape_r2 <- mape_r3 <- mape_r4 <- mape_r5 <- c()

for(i in 1:100) {

mape    <- c(mape,results[[i]]$mape)
mape_r1 <- c(mape_r1,results_r1[[i]]$mape)
mape_r2 <- c(mape_r2,results_r2[[i]]$mape)
mape_r3 <- c(mape_r3,results_r3[[i]]$mape)
mape_r4 <- c(mape_r4,results_r4[[i]]$mape)
mape_r5 <- c(mape_r5,results_r5[[i]]$mape)
  
}

write.csv(mape,file=paste('ffnn_mape_t_',zone_names[z],'_',temp_names[t],'.csv',sep=''))
write.csv(mape_r1,file=paste('ffnn_mape_r1_',zone_names[z],'_',temp_names[t],'.csv',sep=''))
write.csv(mape_r2,file=paste('ffnn_mape_r2_',zone_names[z],'_',temp_names[t],'.csv',sep=''))
write.csv(mape_r3,file=paste('ffnn_mape_r3_',zone_names[z],'_',temp_names[t],'.csv',sep=''))
write.csv(mape_r4,file=paste('ffnn_mape_r4_',zone_names[z],'_',temp_names[t],'.csv',sep=''))
write.csv(mape_r5,file=paste('ffnn_mape_r5_',zone_names[z],'_',temp_names[t],'.csv',sep=''))

}

for(t in 1:11) {
  ffnn_predicts_train <-  c()
  ffnn_predicts <- ffnn_predicts_r1 <- ffnn_predicts_r2 <- ffnn_predicts_r3 <- ffnn_predicts_r4 <- ffnn_predicts_r5 <- c()
  for(i in 1:100) {
    ffnn_predicts_train    <- cbind(ffnn_predicts_train,as.vector(t(predicts_train[[t]][[i]])))
    
    ffnn_predicts    <- cbind(ffnn_predicts,as.vector(t(predicts[[t]][[i]])))
    ffnn_predicts_r1 <- cbind(ffnn_predicts,as.vector(t(predicts_r1[[t]][[i]])))
    ffnn_predicts_r2 <- cbind(ffnn_predicts,as.vector(t(predicts_r2[[t]][[i]])))
    ffnn_predicts_r3 <- cbind(ffnn_predicts,as.vector(t(predicts_r3[[t]][[i]])))
    ffnn_predicts_r4 <- cbind(ffnn_predicts,as.vector(t(predicts_r4[[t]][[i]])))
    ffnn_predicts_r5 <- cbind(ffnn_predicts,as.vector(t(predicts_r5[[t]][[i]])))
  }
  write.csv(ffnn_predicts_train,file=paste('ffnn_predicts_train_',zone_names[z],'_',temp_names[t],'.csv',sep=''))

  write.csv(ffnn_predicts,file=paste('ffnn_predicts_',zone_names[z],'_',temp_names[t],'.csv',sep=''))
  write.csv(ffnn_predicts_r1,file=paste('ffnn_predicts_',zone_names[z],'_',temp_names[t],'_r1.csv',sep=''))
  write.csv(ffnn_predicts_r2,file=paste('ffnn_predicts_',zone_names[z],'_',temp_names[t],'_r2.csv',sep=''))
  write.csv(ffnn_predicts_r3,file=paste('ffnn_predicts_',zone_names[z],'_',temp_names[t],'_r3.csv',sep=''))
  write.csv(ffnn_predicts_r4,file=paste('ffnn_predicts_',zone_names[z],'_',temp_names[t],'_r4.csv',sep=''))
  write.csv(ffnn_predicts_r5,file=paste('ffnn_predicts_',zone_names[z],'_',temp_names[t],'_r5.csv',sep=''))
}
}