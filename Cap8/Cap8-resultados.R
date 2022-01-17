z_names <- c('CAPITL','CENTRL','GENESE','HUDVL','NORTH','WEST')

z <- 6
  
setwd(paste('~/PGMC - 2017-2021/Tese - parte 2/Códigos/',z_names[z],sep=''))

library(ggplot2)
library(pastecs)
library(readr)

t_names <- c('ALB','ART','BGM','BUF','HPN','JFK','LGA','MSS','RME','ROC','SYR')

t <- 1

ffnn_mape    <- read_csv(paste('ffnn_mape_t_ ',t,' .csv',sep=''))
ffnn_mape_r1 <- read_csv(paste('ffnn_mape_r1_t_ ',t,' .csv',sep=''))
ffnn_mape_r2 <- read_csv(paste('ffnn_mape_r2_t_ ',t,' .csv',sep=''))
ffnn_mape_r3 <- read_csv(paste('ffnn_mape_r3_t_ ',t,' .csv',sep=''))
ffnn_mape_r4 <- read_csv(paste('ffnn_mape_r4_t_ ',t,' .csv',sep=''))
ffnn_mape_r5 <- read_csv(paste('ffnn_mape_r5_t_ ',t,' .csv',sep=''))

data <- data.frame(c(ffnn_mape$x,
                     ffnn_mape_r1$x,
                     ffnn_mape_r2$x,
                     ffnn_mape_r3$x,
                     ffnn_mape_r4$x,
                     ffnn_mape_r5$x),
                   c(rep('r=0',times=100),
                     rep('r=1%',times=100),
                     rep('r=2%',times=100),
                     rep('r=3%',times=100),
                     rep('r=4%',times=100),
                     rep('r=5%',times=100)),
                   rep(t_names[t],times=100))

names(data)<-c('MAPE','r','t')

for(t in 2:11) {

ffnn_mape    <- read_csv(paste('ffnn_mape_t_ ',t,' .csv',sep=''))
ffnn_mape_r1 <- read_csv(paste('ffnn_mape_r1_t_ ',t,' .csv',sep=''))
ffnn_mape_r2 <- read_csv(paste('ffnn_mape_r2_t_ ',t,' .csv',sep=''))
ffnn_mape_r3 <- read_csv(paste('ffnn_mape_r3_t_ ',t,' .csv',sep=''))
ffnn_mape_r4 <- read_csv(paste('ffnn_mape_r4_t_ ',t,' .csv',sep=''))
ffnn_mape_r5 <- read_csv(paste('ffnn_mape_r5_t_ ',t,' .csv',sep=''))

data2 <- data.frame(c(ffnn_mape$x,
                     ffnn_mape_r1$x,
                     ffnn_mape_r2$x,
                     ffnn_mape_r3$x,
                     ffnn_mape_r4$x,
                     ffnn_mape_r5$x),
                   c(rep('r=0',times=100),
                     rep('r=1%',times=100),
                     rep('r=2%',times=100),
                     rep('r=3%',times=100),
                     rep('r=4%',times=100),
                     rep('r=5%',times=100)),
                   rep(t_names[t],times=100))

names(data2)    <- names(data)

data <- rbind(data,data2)

names(data)    <- c('MAPE','r','t')

}

lineares_mape  <- read_csv('lineares_mape.csv')

lineares_mape <- lineares_mape[which(lineares_mape$X1==z_names[z]),]

ggplot(data, aes(x=r, y=MAPE)) +
  geom_boxplot() +
  facet_wrap( ~ t, scales="free") +
  geom_hline(yintercept=lineares_mape$Naive24, linetype="solid", color = "red") +
  geom_hline(yintercept=lineares_mape$Naive168, linetype="solid", color = "blue") +
  geom_hline(yintercept=lineares_mape$HWT, linetype="solid", color = "green") +
  geom_hline(yintercept=lineares_mape$Sarima, linetype="solid", color = "purple") +
  ggtitle(z_names[z]) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

stats <- cbind(aggregate(data[, 1], list(data$t), mean),
               aggregate(data[, 1], list(data$t), median)[2],
               aggregate(data[, 1], list(data$t), min)[2],
               aggregate(data[, 1], list(data$t), max)[2],
               aggregate(data[, 1], list(data$t), sd)[2])

names(stats) <- c('t','Média','Mediana','Mínimo','Máximo','Desvio-padrão')

write.csv(stats,file=paste('stats_',z_names[z],'.csv',sep=''),row.names = FALSE)
