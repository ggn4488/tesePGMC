# Tese - Códigos do Capítulo 5 --------------------------------------------

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------
library(readr)
library(ggplot2)
library(cowplot)
library(showtext)
library(scales)
library(tidyverse)
library(TTR)
library(plyr)
library(MLmetrics)

#--------------------------------------------------
# Muda diretório de trabalho ----
#--------------------------------------------------
setwd("C:/Users/Pc/Documents/GitHub/tesePGMC/Cap2")

#--------------------------------------------------
# Adiciona fontes ----
#--------------------------------------------------

font_add("lm10", regular = "lmroman10-regular.otf",
         bold = "lmroman10-Bold.otf")

showtext_auto()

#--------------------------------------------------
# Tema para os gráficos ----
#--------------------------------------------------
tema_graficos_1 <- theme(axis.title.x = element_text(size=14, family="lm10", face = 'bold'),
                         axis.title.y = element_text(size=20, family="lm10", face = 'bold'),
                         axis.line = element_line(size = 1),
                         axis.text.x = element_text(size=12, family="lm10"),
                         axis.text.y = element_text(size=12, family="lm10"),
                         legend.text = element_text(size=12, family="lm10"),
                         legend.title = element_text(size=0, family="lm10"),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         panel.background = element_blank(), 
                         legend.key = element_blank())

#------------------------------------------------------------------------------
# Fig. 5.1: Diagrama de dispersão carga x temperatura (médias) para WEST e BUF ----
#------------------------------------------------------------------------------

cargas <- read_delim("cargas_ny_1518_untidy.csv", ",")

colnames(cargas)[6] <- 'MHKVL'
colnames(cargas)[8] <- 'HUDVL'

cargas_medias <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL, HUDVL) ~ Dias + Estacao, data = cargas, FUN = mean, na.rm = TRUE)

temps <- read_delim("temps_ny_1518_untidy.csv", ",")

temps$Estacao <- cargas$Estacao
temps$Dias <- cargas$Dias

temps_medias <- aggregate(cbind(ALB, ART, BGM, BUF, ELM, HPN, JFK, LGA, MSS, MSV, PBG, POU, RME, ROC, SYR, SWF) ~ Dias + Estacao, data = temps, FUN = mean, na.rm = TRUE)

cargas_temps <- cbind(cargas_medias, temps_medias[,3:18])

cargas_temps$Estacao <- factor(cargas_temps$Estacao)
cargas_temps$Estacao <- revalue(cargas_temps$Estacao, c("1" = "Primavera", "2"="Verão", "3" = "Outono", "4"="Inverno"))

cargas_temps <- cargas_temps %>% filter(Estacao == "Primavera")

set.seed(123)

index <- sample(1:(nrow(cargas_temps)-1), size=1)
amostra_treino <- cargas_temps[index:(index+29), ]

fig5_1 <- ggplot(amostra_treino, aes(x = BUF, y = WEST)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)") +
  geom_point(alpha = 0.8, size = 2.5) +
  tema_graficos_1

print(fig5_1)

dev.off()

#------------------------------------------------------------------------------
# Fig. 5.2: Regressão linear ----
#------------------------------------------------------------------------------
modelo_rl <- lm(WEST~BUF, data = amostra_treino)

amostra_treino$reta <- predict(modelo_rl, data.frame(amostra_treino))

fig5_2 <- ggplot(amostra_treino, aes(x = BUF, y = WEST)) +
  geom_line(aes(x = BUF, y = reta)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)") +
  geom_point(alpha = 0.8, size = 2.5) + 
  geom_segment(aes(xend= BUF,yend=reta), linetype = "dashed" , show.legend=T) +
  tema_graficos_1

print(fig5_2)

dev.off()

mape_rl1 <- MAPE(amostra_treino$reta, amostra_treino$WEST)*100

#------------------------------------------------------------------------------
# Fig. 5.3: Regressão polinomial ----
#------------------------------------------------------------------------------

modelo_rl_2 <- lm(WEST ~ I(BUF^1) + I(BUF^2) + I(BUF^3) + I(BUF^4) + I(BUF^5) + I(BUF^6) +I(BUF^7), data = amostra_treino)

amostra_treino$polinomio <- predict(modelo_rl_2,data.frame(amostra_treino))

fig5_3 <- ggplot(amostra_treino, aes(x = BUF, y = WEST)) +
  geom_line(aes(x = BUF, y = polinomio)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)") +
  geom_point(alpha = 0.8, size = 2.5) + 
  geom_segment(aes(xend= BUF,yend=polinomio), linetype = "dashed" , show.legend=T) +
  tema_graficos_1

print(fig5_3)

mape_rl2 <- MAPE(amostra_treino$polinomio, amostra_treino$WEST)*100

dev.off()