# Tese - C�digos do Cap�tulo 2 --------------------------------------------

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
library(stringr)

#--------------------------------------------------
# Muda diret�rio de trabalho ---
#--------------------------------------------------
setwd("~/PGMC 31-07-2020 on/Tese/C�digo/Cap2")

#--------------------------------------------------
# Adiciona fontes ----
#--------------------------------------------------

font_add("lm10", regular = "lmroman10-regular.otf",
         bold = "lmroman10-Bold.otf")

showtext_auto()

#--------------------------------------------------
# Tema para os gr�ficos ----
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
# Fig. 2.3: Boxplot para cargas, por zona ----
#--------------------------------------------------
# L� arquivo csv ----
cargas_tidy <- read.csv('cargas_ny_1518_tidy.csv')

# Filtra zonas desejadas ----
cargas_tidy_filtro <- cargas_tidy %>% filter(!(Zona %in% c('MILLWD', 'DUNWOD', 'LONGIL', 'N.Y.C.') ))

# Recodifica fatores, mudando r�tulos ----
cargas_tidy_filtro$Zona <- recode_factor(cargas_tidy_filtro$Zona, WEST = "A - WEST",
                                         GENESE = "B - GENESE",
                                         CENTRL = "C - CENTRL",
                                         NORTH = "D - NORTH",
                                         'MHK VL' = "E - MHKVL",
                                         CAPITL = "F - CAPITL",
                                         'HUD VL' = "G - HUDVL")
                                         
# Remove objetos que n�o ser�o mais usados ----

rm(cargas_tidy)

# Cria objeto para o gr�fico ----
f2_3 <- ggplot(cargas_tidy_filtro, aes(x = Zona, y = Carga)) +
  labs(x = "Zona de carga", y = "Carga (MW)", color = '') +
  geom_boxplot() + 
  tema_graficos_1 +
  theme(axis.ticks.x = element_blank())

# Mostra o gr�fico ----
print(f2_3)

# Mostra na tela ----

print(f2_3)   

# Desliga ----

dev.off()

# Remove objetos ----

rm(cargas_tidy_filtro)
rm(f2_3)

#--------------------------------------------------
# Estat�sticas descritivas (Tabela 2.3) ----
#--------------------------------------------------
# Importa dados .csv ----
cargas_tidy <- read.csv('cargas_ny_1518_tidy.csv')

# Filtra zonas de carga
cargas_tidy_filtro <- cargas_tidy %>% filter(!(Zona %in% c('MILLWD', 'DUNWOD', 'LONGIL', 'N.Y.C.') ))

# Recodifica fatores, mudando r�tulos ----
cargas_tidy_filtro$Zona <- recode_factor(cargas_tidy_filtro$Zona, WEST = "A - WEST",
                                         GENESE = "B - GENESE",
                                         CENTRL = "C - CENTRL",
                                         NORTH = "D - NORTH",
                                         'MHK VL' = "E - MHKVL",
                                         CAPITL = "F - CAPITL",
                                         'HUD VL' = "G - HUDVL")

# Nomes das zonas de carga ----
nomes_zonas <- unique(cargas_tidy_filtro$Zona)
nomes_zonas_c <- str_sort(nomes_zonas)

# Estat�sticas descritivas
descritivas_carga <- c()

for(z in nomes_zonas_c) {
  
  # Estat�sticas descritivas
  carga_zona <- cargas_tidy_filtro %>% filter(Zona == z)
  
  descritivas_carga <- rbind(descritivas_carga, stat.desc(carga_zona$Carga))
  
}

descritivas_carga      <- data.frame(descritivas_carga)
descritivas_carga$Zona <- nomes_zonas_c

# Detec��o de outliers (mudar conforme a zona de carga) ----
z                     <- nomes_zona[1]
cargas_zona           <- cargas_tidy_filtro %>% filter(Zona == z)
outliers              <- boxplot.stats(carga_zona$Carga)$out
cargas_outliers       <- carga_zona[carga_zona$Carga %in% outliers,]
numero_dias_outliers  <- unique(carga_outliers$Dias)

#--------------------------------------------------
# Fig. 2.5: Cargas m�dias di�rias para as zonas do NYISO, anos de 2015 a 2018. ----
#--------------------------------------------------

# L� arquivo csv ----

cargas_ny <- read_delim("cargas_ny_1518_untidy.csv", ",")

# Calcula m�dias di�rias para cada zona de carga ----

names(cargas_ny)[6] <- 'HUDVL'
names(cargas_ny)[8] <- 'MHKVL'
medias_diarias_ny <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, CAPITL, MHKVL, HUDVL) ~ Dias, data = cargas_ny, FUN = mean, na.rm = TRUE)

# Calcula m�dias m�veis simples (janela 7) ----

mm_ny <- data.frame(medias_diarias_ny[1], 
                    apply(select(medias_diarias_ny, WEST:HUDVL), 2, SMA, n = 7))

# Reescreve o data frame em formato tidy ----

mm_ny_tidy <- mm_ny %>%
  select(Dias, WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL, HUDVL) %>%
  gather(key = "Zona", value = "Carga", -Dias)

mm_ny_tidy$Zona <- factor(mm_ny_tidy$Zona, levels= c("WEST", "GENESE", "CENTRL", "NORTH", "MHKVL", "CAPITL", 'HUDVL'))
mm_ny_tidy$Zona <- revalue(mm_ny_tidy$Zona, c("WEST" = "A - WEST", "GENESE" = "B - GENESE", "CENTRL" = "C - CENTRL", "NORTH" = "D - NORTH", "MHKVL" = "E - MHKVL", "CAPITL" = "F - CAPITL", "HUDVL" = "G - HUDVL"))

# Remove objetos que n�o ser�o mais usados ----

rm(cargas_ny)
rm(medias_diarias_ny)
rm(mm_ny)

# Cria objeto para o gr�fico ----

f2_5 <- ggplot(mm_ny_tidy, aes(x = Dias, y = Carga)) +
  labs(x = "", y = "Log10 da Carga m�dia di�ria (MW)", color = '') +
  geom_line(aes(color = Zona), size = 1.2) + 
  tema_graficos_1 +
  theme(axis.text.x = element_text(size=0,family="lm10")) +
  theme(axis.ticks.x = element_blank()) +
  scale_x_continuous(limits=c(1,1461), breaks=seq(1, 1461, 31), minor_breaks=seq(0,1461,31), expand = c(0, 0)) +
  scale_color_manual(values=c("A - WEST"="#9a5fe7", "B - GENESE"="#6ef8a9", "C - CENTRL"="#5cc9ec", "D - NORTH" = "#e8af73", "E - MHKVL" = "#e87965", "F - CAPITL" = "#5ba498", "G - HUDVL" = "#003f5c")) +
  scale_y_continuous(trans='log10')

# Mostra na tela ----

print(f2_5)   

# Desliga ----

dev.off()

# Remove objetos ----

rm(mm_ny_tidy)
rm(f2_5)

#--------------------------------------------------
# Fig. 2.6: Cargas de duas semanas de inverno e duas de ver�o, amostradas por conveni�ncia (zona de carga A - WEST)
#--------------------------------------------------
# L� arquivo csv ----
cargas <- read_delim("cargas_ny_1518_untidy.csv", ",")

# Filtra para esta��o CENTRL e para inverno e ver�o ----

cargas <- cargas %>%
                    select(Data, DiaJuliano, DiaSemana, Hora, Estacao, CENTRL) %>%
                          filter(cargas$Estacao == 2 | cargas$Estacao == 4)

# Seleciona amostra por conveni�ncia
# Datas escolhidas por conveni�ncia: 03/08 a 16/08 de 2015 para o ver�o; 02/02/2015 a 15/02/2015 para o inverno.
cargas <- cargas[c(769:1104, 2911:3246),]

# Converte esta��o do ano para fator ----
cargas$Estacao <- factor(cargas$Estacao, levels = c(2,4))

# �ndice para as horas ----
cargas$index <- c(1:336, 1:336)

# Muda o nome das esta��es, para aparecerem na legenda do gr�fico ----

cargas$Estacao <- revalue(cargas$Estacao, c("2"="Ver�o", "4"="Inverno"))

# Cria objeto para o gr�fico ----

fig2_6 <- ggplot(cargas, aes(x = index, y = CENTRL)) +
  labs(x = "", y = "Carga (MW)", color = '') +
  geom_line(aes(color = Estacao), size = 1.2) + 
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
# Fig. 2.7: Cargas versus temperaturas, m�dias di�rias, para esta��o meteorol�gica BUF e zona de carga A - WEST)
#--------------------------------------------------
# L� arquivos csv ----

cargas <- read_delim("cargas_ny_1518_untidy.csv", ",")
temps <- read_delim("temps_ny_1518_untidy.csv", ",")

names(cargas)[6] <- 'HUDVL'
names(cargas)[8] <- 'MHKVL'

# Calcula cargas m�dias di�rias por esta��o (considerando que alguns dias tem duas esta��es) ----
cargas_agg <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL, HUDVL) ~ Dias + Estacao, data = cargas, FUN = mean, na.rm = TRUE)

# Acrescenta colunas de esta��o e dia ao data frame de temperaturas ----
temps$Estacao <- cargas$Estacao
temps$Dias <- cargas$Dias

# Calcula temperaturas m�dias di�rias por esta��o (considerando que alguns dias tem duas esta��es) ----
temps_agg <- aggregate(cbind(ALB, ART, BGM, BUF, ELM, HPN, JFK, LGA, MSS, MSV, PBG, POU, RME, ROC, SWF, SYR) ~ Dias + Estacao, data = temps, FUN = mean, na.rm = TRUE)

# Cria data frame completo ----
df <- cbind(cargas_agg, temps_agg[,3:18])

# Transforma esta��o do ano em fator e renomeia ----
df$Estacao <- factor(df$Estacao)
df$Estacao <- revalue(df$Estacao, c("1" = "Primavera", "2"="Ver�o", "3" = "Outono", "4"="Inverno"))

# Remove  objetos que n�o s�o mais necess�rios ----

rm(cargas)
rm(temps)
rm(cargas_agg)
rm(temps_agg)

# Cria objeto para o gr�fico ----
f2_7 <- ggplot(df, aes(x = BUF, y = WEST)) +
  labs(x = "Temperatura m�dia di�ria (�C)", y = "Carga m�dia di�ria (MW)", color = '') +
  geom_point(aes(color = Estacao), alpha = 0.8, size = 2.5) + 
  tema_graficos_1 +
  scale_color_manual(values=c("Primavera" = "#ef5675", "Ver�o" = "#ffa600", "Outono" = "#7a5195", "Inverno" = "#d10000"))

# Mostra na tela ----
print(f2_7)

# Desliga ----
dev.off()

# Remove objetos ----

rm(df)
rm(fig2_7)

#--------------------------------------------------
# Fig. 2.8: Cargas versus temperaturas, m�dias di�rias, para esta��o meteorol�gica BUFe zona de carga A - WEST); diagramas separados por esta��o do ano
#--------------------------------------------------
# L� arquivos csv ----

cargas <- read_delim("cargas_ny_1518_untidy.csv", ",")
temps <- read_delim("temps_ny_1518_untidy.csv", ",")

names(cargas)[6] <- 'HUDVL'
names(cargas)[8] <- 'MHKVL'

# Calcula cargas m�dias di�rias por esta��o (considerando que alguns dias tem duas esta��es) ----
cargas_agg <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL, HUDVL) ~ Dias + Estacao, data = cargas, FUN = mean, na.rm = TRUE)

# Acrescenta colunas de esta��o e dia ao data frame de temperaturas ----
temps$Estacao <- cargas$Estacao
temps$Dias <- cargas$Dias

# Calcula temperaturas m�dias di�rias por esta��o (considerando que alguns dias tem duas esta��es) ----
temps_agg <- aggregate(cbind(ALB, ART, BGM, BUF, ELM, HPN, JFK, LGA, MSS, MSV, PBG, POU, RME, ROC, SWF, SYR) ~ Dias + Estacao, data = temps, FUN = mean, na.rm = TRUE)

# Cria data frame completo ----
df <- cbind(cargas_agg, temps_agg[,3:18])

# Transforma esta��o do ano em fator e renomeia ----
df$Estacao <- factor(df$Estacao)
df$Estacao <- revalue(df$Estacao, c("1" = "Primavera", "2"="Ver�o", "3" = "Outono", "4"="Inverno"))

# Remove  objetos que n�o s�o mais necess�rios ----

rm(cargas)
rm(temps)
rm(cargas_agg)
rm(temps_agg)

# Cria objeto para a figura ----

fig2_8 <- ggplot(df, aes(x = BUF, y = WEST)) +
  labs(x = "Temperatura m�dia di�ria (�C)", y = "Carga m�dia di�ria (MW)", color = '') +
  geom_point(alpha = 0.8, size = 2.5) + 
  facet_wrap(~ Estacao) +
  theme(strip.text.x = element_text(size=18, family="lm10", face = 'bold')) +
  theme(strip.text = element_text(size = 20)) +
  tema_graficos_1

# Mostra na tela ----

print(fig2_8)

# Desliga ----

dev.off()

# Remove objetos ----

rm(fig2_8)
rm(df)

#--------------------------------------------------
# Tabela 2.3: Medidas descritivas, em �C, para temperaturas da esta��o meteorol�gica BUF ----
#--------------------------------------------------
# L� arquivo csv ----

cargas <- read_delim("cargas_ny_1518_untidy.csv", ",")
temps <- read_delim("temps_ny_1518_untidy.csv", ",")

# L� arquivo csv ----

descritivas_temps <- describeBy(temps$BUF, cargas$Estacao)

descritivas_estacao <- c(descritivas$`1`$min, descritivas$`1`$max, descritivas$`1`$median, descritivas$`1`$mean, descritivas$`1`$sd)
descritivas_estacao <- rbind(descritivas_estacao, c(descritivas$`2`$min, descritivas$`2`$max, descritivas$`2`$median, descritivas$`2`$mean, descritivas$`2`$sd))
descritivas_estacao <- rbind(descritivas_estacao, c(descritivas$`3`$min, descritivas$`3`$max, descritivas$`3`$median, descritivas$`3`$mean, descritivas$`3`$sd))
descritivas_estacao <- rbind(descritivas_estacao, c(descritivas$`4`$min, descritivas$`4`$max, descritivas$`4`$median, descritivas$`4`$mean, descritivas$`4`$sd))

descritivas_estacao <- data.frame(descritivas_estacao)

colnames(descritivas_estacao) <- c('M�nimo', 'M�ximo', 'Mediana', 'M�dia', 'Desvio padr�o')
rownames(descritivas_estacao) <- c('Primavera', 'Ver�o', 'Outono', 'Inverno')

setwd("~/PGMC 31-07-2020 on/Tese/C�digo/DataPrep/ny_descritivas/")

write.csv(descritivas_estacao, "descritivas_temps_BUF.csv")

# Cria data frame para todos os dados ----

df <- cbind(cargas, temps)

# Coeficiente de correla��o entre carga e temperatura por esta��o do ano ----
# obs.: o ddply n�o permite fazer o loop nas colunas; a zona de carga e a esta��o meteorol�gica devem ser trocadas manualmente

ddply(df, .(Estacao), summarise, "corr" = cor(BUF,WEST))

# Remove objetos ----

rm(cargas)
rm(temps)