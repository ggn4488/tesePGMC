# Tese - C�digos do Cap�tulo 1 --------------------------------------------

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(showtext)
library(scales)
library(weathermetrics)


#--------------------------------------------------
# Muda diret�rio de trabalho ----
#--------------------------------------------------

setwd("C:/Users/Pc/Documents/PGMC 31-07-2020 on/Tese/Cap1")

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
# Fig. 1.2: Temperaturas globais anuais m�dias e curva de tend�ncia (obtida pelo m�todo LOESS ----
#--------------------------------------------------

# L� arquivo csv ----

global_temps <- read_delim("global-temperature.csv", " ", 
                 escape_double = FALSE, trim_ws = TRUE)

# D� nomes �s colunas ----

colnames(global_temps) <- c('ano', 'm�dia', 'tend�ncia')

# Reescreve em formato tidy ----

gt_tidy <- global_temps %>%  pivot_longer(m�dia:tend�ncia, 
                                          names_to = "label", 
                                          values_to = "temp")
# Remove o data frame antigo ----

rm(global_temps)

# Cria o gr�fico em objeto ggplot ----

fig1_2 <- ggplot(gt_tidy, aes(x = ano, y = temp, group = label)) +
  labs(x = "Ano", y = "Temperatura global (�C)", color = '') +
  geom_line(aes(color = label), size=1.2) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_x_continuous(breaks=seq.int(1880,2020,20)) +
  scale_color_manual(values=c("#ffa600", "#000000")) +
  background_grid() +
  tema_graficos_1

# Mostra na tela ----

print(fig1_2)

# Desliga ----

dev.off()

# Remove objetos ----

rm(fig1_2)
rm(gt_tidy)

#--------------------------------------------------
# Fig. 1.3: Popula��o mundial entre 1950 e 2019 ----
#--------------------------------------------------

# L� arquivo csv ----

pop_mundial <- read_delim("urbana-rural.csv", ";", 
                 escape_double = FALSE, trim_ws = TRUE)

# Reescreve o data frame em formato tidy ----

pm_tidy <- pop_mundial %>% pivot_longer(Urbana:Rural,
                                        names_to = "label",
                                        values_to = "pop")
# Remove o data frame antigo ----

rm(pop_mundial)

# Muda a unidade de medida para 10^6 habitantes ----

pm_tidy <- pm_tidy %>% mutate(pop = pop/(10^6))

# Filtra ano de 2007 para destacar no gr�fico ----

pm_2007 <- pm_tidy %>% 
  filter(Ano == 2007)

# Cria o gr�fico em objeto ggplot ----

fig1_3 <- ggplot(pm_tidy, aes(x = Ano, y = pop, group = label)) +
  labs(x = "Ano", y = "Popula��o mundial (bilh�es)", color = '') +
  geom_line(aes(color = label), size=1.05) + 
  geom_point(data=pm_2007, 
             aes(x=Ano,y=pop),
             size=3) +
  scale_x_continuous(breaks=seq.int(1950,2020,10)) +
  scale_color_manual(values=c("#32228c", "#ffa600")) +
  background_grid() +
  tema_graficos_1

# Mostra o gr�fico na tela ----

fig1_3

# Desliga ----

dev_off()

# Remove objetos ----

rm(fig1_3)
rm(pm_tidy)

#--------------------------------------------------
# Fig. 1.4: Cargas hor�rias para tr�s quartas-feiras seguidas (escolhidas por conveni�ncia), sendo 29/08/1997 um feriado (dados da Eslov�quia) ----
#--------------------------------------------------

# L� arquivo xls e transp�e ----

eunite_97 <- read_excel("Load1997.xls")
eunite_97 <- as.vector(t(as.matrix(eunite_97)))

# Cria �ndices para os dias ----

index_dias <- c()

for(i in 1:365) {
  a <- rep(i, times = 48)
  index_dias <- c(index_dias, a)
}

# Cria data frame ----

cargas <- data.frame(eunite_97, index_dias)
colnames(cargas) <- c('carga', 'dia')

# Cria �ndice para o feriado nacional da revolta da Eslov�quia ----

feriado <- 241

# Cria os �ndices para a amostra por conveni�ncia ----

index_amostra <- c(feriado-7,feriado,feriado+7)

# Faz a amostragem por conveni�ncia ----

amostra_cargas <- cargas %>% 
                    filter(dia == index_amostra[1] | dia == index_amostra[2] | dia == index_amostra[3])

amostra_cargas <- amostra_cargas %>% 
                                  mutate(dia = replace(dia, dia == index_amostra[1], '22/08/1997')) %>% 
                                    mutate(dia = replace(dia, dia == index_amostra[2], '29/08/1997 (feriado)')) %>% 
                                      mutate(dia = replace(dia, dia == index_amostra[3], '05/09/1997'))

# Cria �ndices para as horas do dia ----

index_horas <- c()

vetor_horas <- seq(as.POSIXct("2017-08-29", tz = "UTC"),
                   as.POSIXct("2017-08-30", tz = "UTC"),
                   by = "30 min")

vetor_horas <- vetor_horas[-1]

index_horas <- rep(vetor_horas, 3)

# Estabelece limites de tempo ----

lims <- as.POSIXct(strptime(c("2017-08-30 00:00","2017-08- 23:30"), format = "%Y-%m-%d %H:%M")) 

# Cria o data frame tidy ----

eunite_tidy <- data.frame(index_horas, amostra_cargas)

colnames(eunite_tidy) <- c('Hora', 'Carga', 'Dia')

# Fatores, para usar no ggplot ----

eunite_tidy$Dia <- factor(eunite_tidy$Dia, levels= c("22/08/1997", "29/08/1997 (feriado)", "05/09/1997"))

# Remove objetos que n�o ser�o mais usados ----

rm(a)
rm(eunite_97)
rm(feriado)
rm(i)
rm(index_amostra)
rm(index_dias)
rm(lims)
rm(vetor_horas)
rm(amostra_cargas)
rm(cargas)

# Cria o gr�fico em objeto ----

fig1_4 <- ggplot(eunite_tidy, aes(x = Hora, y = Carga, group = Dia)) +
  labs(x = "Hora do dia", y = "Carga (MW)", color = '') +
  geom_line(aes(color = Dia), size=1.3) +
  scale_x_datetime(labels = date_format("%H:%M"),date_breaks = "5 hours") +
  scale_color_manual(values=c("22/08/1997"="#003f5c", "29/08/1997 (feriado)"="#ef5675", "05/09/1997"="#7a5195")) +
  background_grid() +
  tema_graficos_1

# Mostra o gr�fico na tela ----

print(fig1_4)

# Desliga ----

dev.off()

# Remove objetos ----

rm(fig1_4)
rm(eunite_tidy)

#--------------------------------------------------
# Fig. 1.5: Cargas hor�rias para tr�s quartas-feiras seguidas (escolhidas por conveni�ncia), sendo 29/08/1997 um feriado (dados da Eslov�quia) ----
#--------------------------------------------------
# L� arquivo xls e transp�e ----

eunite_97 <- read_excel("Load1997.xls")
eunite_97 <- as.vector(t(as.matrix(eunite_97)))

# Cria �ndices para os dias ----

index_dias <- c()

for(i in 1:365) {
  a <- rep(i, times = 48)
  index_dias <- c(index_dias, a)
}

# Cria data frame ----

cargas <- data.frame(eunite_97, index_dias)
colnames(cargas) <- c('carga', 'dia')

# Cria �ndice para o feriado nacional da revolta da Eslov�quia ----

feriado <- 241

# Cria os �ndices para a amostra por conveni�ncia ----

index_amostra <- c(feriado-7,feriado,feriado+7)

# Faz a amostragem por conveni�ncia ----

amostra_cargas <- cargas %>% 
  filter(dia == index_amostra[1] | dia == index_amostra[2] | dia == index_amostra[3])

# Imputa��o de dados no lugar do feriado ----

amostra_cargas <- amostra_cargas %>% 
  mutate(dia = replace(dia, dia == index_amostra[1], '22/08/1997')) %>% 
  mutate(dia = replace(dia, dia == index_amostra[2], 'dados imputados')) %>% 
  mutate(dia = replace(dia, dia == index_amostra[3], '05/09/1997'))

amostra_cargas <- rbind(amostra_cargas, amostra_cargas[49:96,1:2])

for(i in 49:96) {
  amostra_cargas$carga[i] <- (amostra_cargas$carga[i-48] + amostra_cargas$carga[i+48])/2
}
amostra_cargas$dia[145:192] <- "29/08/1997 (feriado)"

# Cria �ndices para as horas do dia ----

index_horas <- c()

vetor_horas <- seq(as.POSIXct("2017-08-29", tz = "UTC"),
                   as.POSIXct("2017-08-30", tz = "UTC"),
                   by = "30 min")

vetor_horas <- vetor_horas[-1]

index_horas <- rep(vetor_horas, 4)

# Estabelece limites de tempo ----

lims <- as.POSIXct(strptime(c("2017-08-30 00:00","2017-08- 23:30"), format = "%Y-%m-%d %H:%M")) 

# Cria o data frame tidy ----

eunite_tidy <- data.frame(index_horas, amostra_cargas)

colnames(eunite_tidy) <- c('Hora', 'Carga', 'Dia')

# Fatores, para usar no ggplot ----

eunite_tidy$Dia <- factor(eunite_tidy$Dia, levels= c("22/08/1997", "29/08/1997 (feriado)", "05/09/1997", "dados imputados"))

# Remove objetos que n�o ser�o mais usados ----

rm(a)
rm(eunite_97)
rm(feriado)
rm(i)
rm(index_amostra)
rm(index_dias)
rm(lims)
rm(vetor_horas)
rm(amostra_cargas)
rm(cargas)

# Cria o gr�fico em objeto ----

fig1_5 <- ggplot(eunite_tidy, aes(x = Hora, y = Carga)) +
  labs(x = "Hora do dia", y = "Carga (MW)", color = '', linetype='') +
  geom_line(aes(color = Dia, linetype=Dia, alpha = Dia), size=1.1) + 
  scale_x_datetime(labels = date_format("%H:%M"),date_breaks = "5 hours") +
  scale_linetype_manual(values=c("22/08/1997"="solid", "29/08/1997 (feriado)"="solid", "05/09/1997"="solid", "dados imputados" = "solid",guide = 'none')) +
  scale_color_manual(values=c("22/08/1997"="#003f5c", "29/08/1997 (feriado)"="#ef5675", "05/09/1997"="#7a5195", "dados imputados" = "#f02c1a")) +
  scale_alpha_manual(values=c("22/08/1997"=0.3, "29/08/1997 (feriado)"=0.3, "05/09/1997"=0.3, "dados imputados" = 1),guide = 'none') +
  background_grid() +
  tema_graficos_1

# Mostra na tela ----

print(fig1_5)

# Desliga ----

dev.off()

# Remove objetos ----

rm(eunite_tidy)
rm(fig1_5)

#--------------------------------------------------
# Fig. 1.6: Cargas hor�rias para duas semanas de inverno e duas de ver�o (escolhidas por conveni�ncia; dados da Eslov�quia) ----
#--------------------------------------------------
# L� arquivo xls e transp�e ----

eunite_97 <- read_excel("Load1997.xls")
eunite_97 <- as.vector(t(as.matrix(eunite_97)))

# Cria �ndices para os dias ----

index_dias <- c()

for(i in 1:365) {
  a <- rep(i, times = 48)
  index_dias <- c(index_dias, a)
}

# Cria data frame ----

cargas <- data.frame(eunite_97, index_dias)
colnames(cargas) <- c('carga', 'dia')

# Cria �ndices para os per�odos quente e frio ----

#13/01/1997 (inverno) 15
#07/07/1997 (ver�o) 188

index_quente <- 13
index_frio  <- 188

# Cria �ndices para a amostra por conveni�ncia ----

index_amostra <- c(index_quente:(index_quente+13), index_frio:(index_frio+13))


# Faz a amostragem por conveni�ncia ----

amostra_cargas <- cargas %>% 
  filter(dia %in% index_amostra)

index_hora <- c(1:672, 1:672)

# Cria o data frame tidy ----

eunite_tidy <- data.frame(index_hora, amostra_cargas)

eunite_tidy$dia <- c(rep('Semana de Inverno', 672), rep('Semana de Ver�o', 672))

# Remove objetos que n�o ser�o mais usados ----

rm(a)
rm(eunite_97)
rm(i)
rm(index_amostra)
rm(index_dias)
rm(index_frio)
rm(index_quente)
rm(index_hora)
rm(amostra_cargas)
rm(cargas)

# Cria objeto para a figura ----

fig1_6 <- ggplot(eunite_tidy, aes(x = hora, y = carga)) +
  labs(x = "Dia da Semana", y = "Carga (MW)") +
  geom_line(aes(color = dia), size = 1) + 
  tema_graficos_1 +
  theme(axis.text.x = element_text(size=0,family="lm10")) +
  theme(axis.text.y = element_text(size=0, family="lm10")) +
  theme(legend.title = element_text(size=0, family="lm10")) + 
  scale_color_manual(values=c("#32228c", "#e21464")) +
  scale_x_continuous(limits=c(1,672), breaks=seq(0, 672, 48), minor_breaks=seq(0,672,48), expand = c(0, 0))

# Mostra figura na tela ----

print(fig1_6)

# Desliga ----

dev.off()

# Remove objetos

rm(fig1_6)
rm(eunite_tidy)

#--------------------------------------------------
# Fig. 1.7: Cargas m�dias di�rias para no ano de 1997 (escolhido por conveni�ncia; dados da Eslov�quia)] { - Cargas m�dias di�rias para no ano de 1997 (escolhido por conveni�ncia; dados da Eslov�quia)
#--------------------------------------------------
# L� arquivo xls ----

eunite_97 <- read_excel("Load1997.xls")

# Calcula m�dias di�rias ----

medias <- rowMeans(eunite_97)

# Cria data frame ----

eunite_medias <- data.frame(medias, 1:365)
colnames(eunite_medias) <- c('carga', 'dia')

# Remove objetos que n�o ser�o mais utilizados ----

rm(eunite_97)
rm(medias)

# Cria objeto com a figura ----

fig1_7 <- ggplot(eunite_medias, aes(x = dia, y = carga)) +
  labs(x = "M�s do ano", y = "Carga m�dia di�ria (MW)", color = '') +
  geom_line() + 
  background_grid() +
  tema_graficos_1 +
  scale_x_continuous(limits=c(1,365), breaks=seq(1, 672, 31), minor_breaks=seq(0,672,31), expand = c(0, 0))

# Mostra figura na tela ----

print(fig1_7)

# Desliga ----

dev.off()

# Remove objetos ----

rm(fig1_7)
rm(eunite_medias)

#--------------------------------------------------
# Fig. 1.8: Cargas m�dias di�rias versus temperaturas m�dias di�rias (dados de Puget Sound) ---
#--------------------------------------------------
# Carrega arquivos xls ----

cargas <- read_excel("sharkawi-loads-mean.xlsx")
temps  <- read_excel("sharkawi-temps-mean.xlsx")

# Converte temperaturas de F� para C�

temps <- temps*10
temps <- fahrenheit.to.celsius(temps, round = 2)

# Cria data frame ----

puget <- data.frame(cargas, temps)

colnames(puget) <- c('Carga', 'Temp')

# Remove objetos que n�o ser�o mais usados ----

rm(cargas)
rm(temps)

# Cria objeto com o gr�fico ----

fig1_8 <- ggplot(puget, aes(x = Temp, y = Carga)) +
  labs(x = "Temperatura m�dia di�ria (�C)", y = "Carga m�dia di�ria (MW)", color = '') +
  geom_point(size=.5) +
  background_grid() +
  tema_graficos_1

# Mostra gr�fico na tela ----

print(fig1_8)

# Desliga ----

dev.off()

# Remove objetos ----

rm(fig1_8)
rm(puget)