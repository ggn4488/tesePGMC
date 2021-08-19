# Tese - Códigos do Capítulo 2 --------------------------------------------

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------
library(readr)
library(ggplot2)
library(cowplot)
library(showtext)
library(scales)
library(tidyverse)
library(plyr)
library(TTR)
library(pastecs)
library(psych)
library(dplyr)

#--------------------------------------------------
# Muda diretório de trabalho ----
#--------------------------------------------------

setwd("C:/Users/Pc/Documents/PGMC 31-07-2020 on/Tese/Cap2")


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

#--------------------------------------------------
# Fig. 2.5: Cargas médias diárias para as zonas do NYISO, anos de 2015 a 2018. ----
#--------------------------------------------------

# Lê arquivo csv ----

cargas_ny <- read_delim("loadsny1518.csv", ",")

# Calcula médias diárias para cada zona de carga ----

medias_diarias_ny <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL) ~ day, data = cargas_ny, FUN = mean, na.rm = TRUE)

# Calcula médias móveis simples (janela 7) ----

mm_ny <- data.frame(medias_diarias_ny[1], 
                    apply(select(medias_diarias_ny, WEST:CAPITL), 2, SMA, n = 7))

# Reescreve o data frame em formato tidy ----

mm_ny_tidy <- mm_ny %>%
  select(day, WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL) %>%
  gather(key = "Zona", value = "Carga", -day)

mm_ny_tidy$Zona <- factor(mm_ny_tidy$Zona, levels= c("WEST", "GENESE", "CENTRL", "NORTH", "MHKVL", "CAPITL"))
mm_ny_tidy$Zona <- revalue(mm_ny_tidy$Zona, c("WEST" = "A - WEST", "GENESE" = "B - GENESE", "CENTRL" = "C - CENTRL", "NORTH" = "D - NORTH", "MHKVL" = "E - MHKVL", "CAPITL" = "F - CAPITL"))

# Remove objetos que não serão mais usados ----

rm(cargas_ny)
rm(medias_diarias_ny)
rm(mm_ny)

# Cria objeto para o gráfico ----

f1_5 <- ggplot(mm_ny_tidy, aes(x = day, y = Carga)) +
  labs(x = "", y = "Log10 da Carga média diária (MW)", color = '') +
  geom_line(aes(color = Zona), size = 1.2) + 
  tema_graficos_1 +
  theme(axis.text.x = element_text(size=0,family="lm10")) +
  theme(axis.ticks.x = element_blank()) +
  scale_x_continuous(limits=c(1,1461), breaks=seq(1, 1461, 31), minor_breaks=seq(0,1461,31), expand = c(0, 0)) +
  scale_color_manual(values=c("A - WEST"="#9a5fe7", "B - GENESE"="#6ef8a9", "C - CENTRL"="#5cc9ec", "D - NORTH" = "#e8af73", "E - MHKVL" = "#e87965", "F - CAPITL" = "#5ba498")) +
  scale_y_continuous(trans='log10')

# Mostra na tela ----

print(f1_5)   

# Desliga ----

dev.off()

# Remove objetos ----

rm(mm_ny_tidy)
rm(f1_5)

#--------------------------------------------------
# Tabela 2.2: População e medidas descritivas por zona de carga
#--------------------------------------------------
# Lê arquivo csv ----

cargas <- read_delim("loadsny1518.csv", ",")
cargas <- cargas[1:35040,]

# Estatísticas descritivas ----

stat.desc(cargas[15:25])

# Remove objeto ----

rm(cargas)

#--------------------------------------------------
# Fig. 2.6: Cargas de duas semanas de inverno e duas de verão, amostradas por conveniência (zona de carga A - WEST)
#--------------------------------------------------
# Datas escolhidas por conveniência: 03/08 a 16/08 de 2015 para o verão; 02/02/2015 a 15/02/2015 para o inverno.

# Lê arquivo csv ----
cargas <- read_delim("loadsny1518.csv", ",")

# Filtra para estação CENTRL e para inverno e verão ----

cargas <- cargas %>%
                    select(date, yearday,weekday, season, CENTRL) %>%
                          filter(cargas$season == 2 | cargas$season == 4)

# Converte estação do ano para fator ----

cargas$season <- factor(cargas$season, levels = c(2,4))

# Ordena ----

cargas <- cargas[order(cargas$season),]

# Separa verão e inverno ----

cargas_verao   <- cargas[13:8849,]
cargas_inverno <- cargas[9093:17510,]

# Seleciona amostras por conveniência para verão e inverno ----

k1 <- 6
k2 <- 4

cargas_verao <-   cargas_verao[((1 + 168*k1):(336 + 168*k1)),]
cargas_inverno <- cargas_inverno[((1 + 168*k2):(336 + 168*k2)),]

cargas <- rbind(cargas_verao, cargas_inverno)

# Índices para as horas ----

cargas$index <- c(1:336, 1:336)

# Muda o nome das estações, para aparecerem na legenda do gráfico ----

cargas$season <- revalue(cargas$season, c("2"="Verão", "4"="Inverno"))

# Remove objetos que não serão mais necessários ----

rm(k1)
rm(k2)
rm(cargas_inverno)
rm(cargas_verao)

# Cria objeto para o gráfico ----

fig2_6 <- ggplot(cargas, aes(x = index, y = CENTRL)) +
  labs(x = "", y = "Carga (MW)", color = '') +
  geom_line(aes(color = season), size = 1.2) + 
  tema_graficos_1 +
  theme(axis.text.x = element_text(size=0,family="lm10")) +
  theme(axis.ticks.x = element_blank()) +
  scale_color_manual(values=c("#e21464", "#32228c")) +
  scale_x_continuous(limits=c(1,336), breaks=seq(0, 336, 24), minor_breaks=seq(0,336,24), expand = c(0, 0))

# Mostra figura na tela ----

print(fig2_6)

# Desliga ----

dev.off()

# Remove objetos ----

rm(cargas)
rm(fig2_6)


#--------------------------------------------------
# Fig. 2.7: Cargas versus temperaturas, médias diárias, para estação meteorológica BUF e zona de carga A - WEST)
#--------------------------------------------------
# Lê arquivos csv ----

cargas <- read_delim("loadsny1518.csv", ",")
cargas <- cargas[1:35040,]
temps <- read_delim("tempsny1518.csv", ",")
temps <- temps[1:35040,]

# Calcula cargas médias diárias por estação (considerando que alguns dias tem duas estações) ----
cargas_agg <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL) ~ day + season, data = cargas, FUN = mean, na.rm = TRUE)

# Acrescenta colunas de estação e dia ao data frame de temperaturas ----
temps$season <- cargas$season
temps$day <- cargas$day

# Calcula temperaturas médias diárias por estação (considerando que alguns dias tem duas estações) ----
temps_agg <- aggregate(cbind(ALB, ART, BGM, BUF, HPN, JFK, LGA, MSS, RME, ROC, SYR) ~ day + season, data = temps, FUN = mean, na.rm = TRUE)

# Cria data frame completo ----
df <- cbind(cargas_agg, temps_agg[,3:13])

# Transforma estação do ano em fator e renomeia ----
df$season <- factor(df$season)
df$season <- revalue(df$season, c("1" = "Primavera", "2"="Verão", "3" = "Outono", "4"="Inverno"))

# Remove  objetos que não são mais necessários ----

rm(cargas)
rm(temps)
rm(cargas_agg)
rm(temps_agg)

# Cria objeto para o gráfico ----
f2_7 <- ggplot(df, aes(x = BUF, y = WEST)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)", color = '') +
  geom_point(aes(color = season), alpha = 0.8, size = 2.5) + 
  tema_graficos_1 +
  scale_color_manual(values=c("Inverno" = "#d10000", "Verão" = "#ffa600", "Outono" = "#7a5195","Primavera" = "#ef5675"))

# Mostra na tela ----
print(f2_7)

# Desliga ----
dev.off()

# Remove objetos ----

rm(df)
rm(fig2_7)

#--------------------------------------------------
# Fig. 2.8: Cargas versus temperaturas, médias diárias, para estação meteorológica BUFe zona de carga A - WEST); diagramas separados por estação do ano
#--------------------------------------------------
# Lê arquivos csv ----

cargas <- read_delim("loadsny1518.csv", ",")
cargas <- cargas[1:35040,]
temps <- read_delim("tempsny1518.csv", ",")
temps <- temps[1:35040,]

# Calcula cargas médias diárias por estação (considerando que alguns dias tem duas estações) ----
cargas_agg <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL) ~ day + season, data = cargas, FUN = mean, na.rm = TRUE)

# Acrescenta colunas de estação e dia ao data frame de temperaturas ----
temps$season <- cargas$season
temps$day <- cargas$day

# Calcula temperaturas médias diárias por estação (considerando que alguns dias tem duas estações) ----
temps_agg <- aggregate(cbind(ALB, ART, BGM, BUF, HPN, JFK, LGA, MSS, RME, ROC, SYR) ~ day + season, data = temps, FUN = mean, na.rm = TRUE)

# Cria data frame completo ----
df <- cbind(cargas_agg, temps_agg[,3:13])

# Transforma estação do ano em fator e renomeia ----
df$season <- factor(df$season)
df$season <- revalue(df$season, c("1" = "Primavera", "2"="Verão", "3" = "Outono", "4"="Inverno"))

# Remove  objetos que não são mais necessários ----

rm(cargas)
rm(temps)
rm(cargas_agg)
rm(temps_agg)

# Cria objeto para a figura ----

fig2_8 <- ggplot(df, aes(x = BUF, y = WEST)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)", color = '') +
  geom_point(alpha = 0.8, size = 2.5) + 
  facet_wrap(~ season) +
  theme(strip.text.x = element_text(size=18, family="lm10", face = 'bold')) +
  theme(strip.text = element_text(size = 20)) +
  tema_graficos_1 +
  scale_color_manual(values=c("Inverno" = "#003f5c", "Verão" = "#ffa600", "Outono" = "#7a5195","Primavera" = "#ef5675"))
  
# Mostra na tela ----

print(fig2_8)

# Desliga ----

dev.off()

# Remove objetos ----

rm(fig2_8)
rm(df)

#--------------------------------------------------
# Tabela 2.3: Medidas descritivas, em ºC, para temperaturas da estação meteorológica BUF ----
#--------------------------------------------------
# Lê arquivo csv ----

cargas <- read_delim("loadsny1518.csv", ",")
cargas <- cargas[1:35040,]
temps  <- read_delim("tempsny1518.csv", ",")
temps  <- temps[1:35040,]

# Lê arquivo csv ----

# describeBy(cargas[15:25], cargas$season)

# Cria data frame para todos os dados ----

df <- cbind(cargas, temps)

# Coeficiente de correlação entre carga e temperatura por estação do ano ----
# obs.: o ddply não permite fazer o loop nas colunas; a zona de carga e a estação meteorológica devem ser trocadas manualmente

ddply(df, .(season), summarise, "corr" = cor(BUF,WEST))

# Remove objetos ----

rm(cargas)
rm(temps)
