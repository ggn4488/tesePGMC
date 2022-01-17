setwd("~/PGMC - 2017-2021/Tese - parte 2/Códigos/MLP - Hiperparâmetros")

zone_names   <- c("CAPITL", "CENTRL", "DUNWOD", "GENESE", "HUDVL",  "LONGIL", "MHKVL",  "MILLWD","NORTH",  "N.Y.C.", "WEST")  
zones_wanted <- c(1,2,4,5,7,9,11)

for(z in zones_wanted) {

  lr_results <- read.csv(paste('mlp_lr_',z,'.csv',sep=''))
  lr <- c()

  for(i in 1:11) {

    res     <- lr_results[(4+11*(i-1)):(9+11*(i-1)),2:7]
    res[,1] <- as.numeric(as.character(res[,1]))
    res[,2] <- as.numeric(as.character(res[,2]))
    lr      <- rbind(lr,res[which.min(res[,2]),])

  }

  rownames(lr) <- 1:11
  colnames(lr) <- c('lr','Mediana','Média','Desvio-padrão','Mínimo','Máximo')

  write.csv(lr,paste('mlp_lr_results_',zone_names[z],'.csv',sep=''))

}