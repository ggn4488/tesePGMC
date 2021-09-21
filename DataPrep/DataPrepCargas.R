# Tese - Códigos para preparação dos dados de carga --------------------------------------------

# Obs.: recomendamos executar bloco a bloco.

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(fastDummies)
library(DataCombine)

#--------------------------------------------------
# Muda diretório de trabalho ----
#--------------------------------------------------
setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/")

#--------------------------------------------------
# Carrega todos os .csv para o mesmo Data Frame ----
#--------------------------------------------------
df_tidy <- list.files(path = "~/PGMC 31-07-2020 on/Tese/Código/DataPrep/cargas_ny_originais",    
                 pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                                           
  bind_rows

colnames(df_tidy) <- c('Time', 'Zona', 'PTID', 'Carga') # Renomeia colunas

nome_zonas <- unique(df_tidy$Zona) # Grava nome das zonas de carga

#--------------------------------------------------
# Cria Data Frame untidy para as cargas ----
#--------------------------------------------------
matriz <- c()

for(i in nome_zonas) {
  
  filtro <- df_tidy %>% filter(Zona == i)
  matriz <- cbind(matriz, filtro$Carga)
  
}

df_untidy <- data.frame(matriz)
colnames(df_untidy) <- nome_zonas

#----------------------------------------------------------------
# Inclui data e horário da observação no Data Frame untidy ----
#----------------------------------------------------------------
df_untidy$Time <- df_tidy$Time[1:nrow(df_untidy)]

df_untidy <- df_untidy[c('Time', nome_zonas)]

#--------------------------------------------------
# Inclui coluna com a Data da observação ----
#--------------------------------------------------
df_untidy$Data <- date(df_untidy$Time)

#--------------------------------------------------
# Inclui coluna com a Hora da observação ----
#--------------------------------------------------
df_untidy$Hora <- hour(df_untidy$Time)

#--------------------------------------------------------------
# Passa para formato em que 00:00 é 24:00 no dia anterior ----
#--------------------------------------------------------------
df_untidy$Hora[df_untidy$Hora == 0] <- 24 

df_untidy$Time[1] <- "2014/12/31 23:00:00"

for(i in 2:nrow(df_untidy))  {
  
  if(df_untidy$Hora[i] == 24) {
    
    df_untidy$Time[i] <- df_untidy$Time[i-1]
    
  }
  
}

#--------------------------------------------------------------
# Inclui coluna com dia do ano (calendário juliano) ----
#--------------------------------------------------------------
df_untidy$DiaJuliano <- yday(df_untidy$Time)

#--------------------------------------------------------------
# Inclui coluna com o mês do ano ----
#--------------------------------------------------------------
df_untidy$Mes <- month(df_untidy$Time)

#--------------------------------------------------------------
# Inclui coluna com o dia da semana ----
#--------------------------------------------------------------
df_untidy$DiaSemana <- wday(df_untidy$Time)

#--------------------------------------------------------------
# Passa a coluna "Time" para o formato de hora 24:00 ----
#--------------------------------------------------------------
for(i in 2:nrow(df_untidy))  {
  
  if(df_untidy$Hora[i] == 24) {
    
    df_untidy$Time[i] <- str_replace(df_untidy$Time[i], "23:00:00$", "24:00:00")
    
  }
  
}

#--------------------------------------------------------------
# Inclui coluna com as estações do ano ----
#--------------------------------------------------------------
lista_estacoes <- read.csv("estacoes_ny.csv")

estacoes <- matrix(nrow = 4, ncol = 4)

k <- 1

for(i in 1:4) {
  
  for(j in 1:4) {
    
    estacoes[i,j] <- which(df_untidy$Time == lista_estacoes$Tempo[k])
    k <- k+1
    
  }
}

estacoes <- t(estacoes)

df_untidy$Estacao <- rep(NA, times = nrow(df_untidy))

for(i in 1:4) {
  
  for(j in 1:4) {
    
    if(i == 4 && j == 4) {
      
      df_untidy$Estacao[estacoes[4,4]:nrow(df_untidy)] <- i
      
    } else if(i == 4) {
      
      df_untidy$Estacao[estacoes[i,j]:(estacoes[1,j+1]-1)] <- i
      
    } else {
      
      df_untidy$Estacao[estacoes[i,j]:(estacoes[i+1, j]-1)] <- i

    }
  }
}

df_untidy$Estacao[1:(estacoes[1,1]-1)] <- 4

#--------------------------------------------------------------
# Inclui colunas com dummies para estações do ano ----
#--------------------------------------------------------------
df_untidy <- dummy_cols(df_untidy, select_columns = "Estacao")

#--------------------------------------------------------------
# Inclui coluna com índice da observação ----
#--------------------------------------------------------------
df_untidy$Tendencia <- 1:nrow(df_untidy)

#--------------------------------------------------------------
# Inclui coluna para indicar feriado ----
#--------------------------------------------------------------
df_untidy$Feriado <- rep(0, times = nrow(df_untidy))

lista_feriados <- read.csv("feriados_ny.csv")

for(i in 1:nrow(df_untidy)) {
  
  for(j in lista_feriados$Data) {
    
    if(grepl(j, df_untidy$Time[i])) {
      
      df_untidy$Feriado[i] <- 1
      
    }
  }
}

#--------------------------------------------------------------
# Faz as correções para remover o horário de verão ----
#--------------------------------------------------------------

lista_dst <- read.csv("dst_ny.csv")

# Correção onde é necessário atrasar o relógio
deletar <- c()

for(i in 1:nrow(df_untidy)) {
  for(j in 1:nrow(lista_dst)) {
    if(df_untidy$Time[i] == lista_dst$Tempo[j]) {
      print(df_untidy$Time[i])
      if(lista_dst$Dst[j] == "fim") {
        
        df_untidy[i-2,2:12] <- (df_untidy[i-2, 2:12] + df_untidy[i-1, 2:12])/2
        deletar <- c(deletar, i-1)
        
      }
    }
  }
}

df_untidy <- df_untidy[-deletar,]

# Correção onde é necessário adiantar o relógio

acrescentar <- which(df_untidy$Time %in% lista_dst$Tempo[which(lista_dst$Dst == 'inicio')])

for(k in 1:4) {
  index <- acrescentar[k]
  df_untidy <- InsertRow(df_untidy, NewRow = df_untidy[index,], RowNum = index)
  df_untidy[index + 1,2:12] <- (df_untidy[index,2:12] + df_untidy[index + 2,2:12])/2
  df_untidy$Time[index+1] <- str_replace(df_untidy$Time[index+1], "01:00:00$", "02:00:00")
  df_untidy$Hora[index+1] <- df_untidy$Hora[index] + 1
  
  acrescentar <- which(df_untidy$Time %in% lista_dst$Tempo[which(lista_dst$Dst == 'inicio')])
}


#--------------------------------------------------------------
# Inclui índice para o dia na base de dados ----
#--------------------------------------------------------------
Dias <- c()

for(i in 1:(nrow(df_untidy)/24)) {
  
  Dias <- c(Dias, rep(i, times = 24))
  
}

df_untidy <- df_untidy[-1,]
df_untidy <- rbind(df_untidy, df_untidy[nrow(df_untidy),])
df_untidy$Dias <- Dias

#--------------------------------------------------------------
# Remove a primeira observação e imputa a última ----
#--------------------------------------------------------------
# Isto foi feito porque, no formato em que 00:00 é 24:00 do dia
# anterior, a primeira observação torna-se a última de 2014 (que acabamos excluindo), e
# acaba faltando a última de 2015 (que imputamos com um Naive da hora anterior).

df_untidy$Time[nrow(df_untidy)] <- str_replace(df_untidy$Time[nrow(df_untidy)], "23:00:00$", "24:00:00")

df_untidy$Hora[nrow(df_untidy)] <- 24

df_untidy$Tendencia[nrow(df_untidy)] <- df_untidy$Tendencia[nrow(df_untidy)-1] + 1

df_untidy$Tendencia <- 1:nrow(df_untidy)

#--------------------------------------------------------------
# Gera Data Frame tidy ----
#--------------------------------------------------------------
df_tidy <- df_untidy %>% pivot_longer(all_of(nome_zonas), names_to = "Zona", values_to = "Carga")

#--------------------------------------------------------------
# Remove objetos que não são mais necessários ----
#--------------------------------------------------------------
rm(estacoes)
rm(filtro)
rm(lista_dst)
rm(lista_estacoes)
rm(lista_feriados)
rm(matriz)
rm(acrescentar)
rm(deletar)
rm(Dias)
rm(i)
rm(j)
rm(k)
rm(nome_zonas)

#--------------------------------------------------------------
# Grava arquivos ----
#--------------------------------------------------------------

write.csv(df_tidy,"cargas_ny_1518_tidy.csv", row.names = FALSE)

write.csv(df_untidy, "cargas_ny_1518_untidy.csv", row.names = FALSE)

# https://www.timeanddate.com/calendar/seasons.html?year=2000&n=12
# https://www.public-holidays.us/US_BG_2015_New%20York
# https://www.timeanddate.com/time/change/usa/new-york-state?year=2016

