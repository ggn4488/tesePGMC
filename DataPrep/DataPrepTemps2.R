# Tese - Códigos para preparação dos dados de temperatura --------------------------------------------
# Para estação MSV.

# Obs.: recomendamos executar bloco a bloco.

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(forecast)
library(readr)
library(tidyr)
#--------------------------------------------------
# Pega nome das estações ----
#--------------------------------------------------

setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_originais/")

estacoes <- list.dirs()[-1]
estacoes <- sub(".", "", estacoes)
estacoes <- sub("/", "", estacoes)

t <- 'MSV'

#--------------------------------------------------
# Muda diretório de trabalho ----
#--------------------------------------------------

setwd(paste0(
  "~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_originais/",
  t
))

#--------------------------------------------------
# Carrega todos os .csv para o mesmo Data Frame ----
#--------------------------------------------------

df_1518 <-
  list.files(
    path = paste0(
      "~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_originais/",
      t
    ),
    pattern = "*.csv",
    full.names = TRUE
  ) %>%
  lapply(fread) %>%
  bind_rows

#--------------------------------------------------
# Mantém somente o instante da observação e a temperatura ----
#--------------------------------------------------
df_1518 <- df_1518[, c('DATE', 'TMP')]

#-----------------------------------------------------------
# Converte temperaturas para numérico ----
#-----------------------------------------------------------

df_1518$TMP <- str_replace(df_1518$TMP, ",", ".")

df_1518$TMP <- str_replace(df_1518$TMP, c("A|C|I|M|P|R|U"), "0")

df_1518$TMP <- as.numeric(df_1518$TMP)

df_1518$TMP <- df_1518$TMP / 10

max_t <- max(df_1518$TMP)

#-----------------------------------------------------------
# Inclui colunas para  ano, dia juliano, hora e minuto ----
#-----------------------------------------------------------
df_1518$ano    <- year(df_1518$DATE)
df_1518$dia    <- yday(df_1518$DATE)
df_1518$hora   <- hour(df_1518$DATE)
df_1518$minuto <- minute(df_1518$DATE)
df_1518$index  <- 1:nrow(df_1518)

df_1518 <- na.omit(df_1518)

#-----------------------------------------------------------
# Identifica e remove bogus ----
#-----------------------------------------------------------
# aqui serão removidos reports mesmo horário E 999

indices_duplicatas <-
  which(duplicated(df_1518[, c('ano', 'dia', 'hora')]))

df_duplicatas <- df_1518[indices_duplicatas, ]

df_duplicatas_NA <- df_duplicatas %>% filter(TMP == max_t)

df_1518 <- df_1518 %>% filter(!(index %in% df_duplicatas$index))

#---------------------------------------------------
# Verifica se há valores faltantes registrados ----
#---------------------------------------------------

indices_NA <- df_1518$index[which(df_1518$TMP == max_t)]

setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_prep/clima_ny_faltantes/")
write.csv(
  data.frame(df_duplicatas_NA),
  file = paste0('MSV', "_faltantes.csv"),
  row.names = FALSE
)

df_1518$TMP[which(df_1518$TMP == max_t)] <- NA

#---------------------------------------------------
# Interpola faltantes registrados ----
#---------------------------------------------------

df_1518$TMP <- na.interp(df_1518$TMP)

#---------------------------------------
# Verifica se há valores faltantes ----
#---------------------------------------

# ano, dia e hora dos valores faltantes

adh_faltantes <- c()

k <- 0

for (a in 2015:2018) {
  for (d in 1:365) {
    for (h in 0:23) {
      teste <-
        which((df_1518$dia == d) &
                (df_1518$hora == h) & (df_1518$ano == a))
      
      if (length(teste) == 0) {
        adh_faltantes <- c(adh_faltantes, c(a, d, h))
        k <- k + 1
        
        
      }
      
    }
  }
}

for (h in 0:23) {
  teste <-
    which((df_1518$dia == 366) &
            (df_1518$hora == h) & (df_1518$ano == 2016))
  
  if (length(teste) == 0) {
    adh_faltantes <- c(adh_faltantes, c(2016, 366, h))
    k <- k + 1
    
  }
  
}

for (d in 1:135) {
  for (h in 0:23) {
    teste <-
      which((df_1518$dia == 1) &
              (df_1518$hora == 4) & (df_1518$ano == 2015))
    
    if (length(teste) == 0) {
      adh_faltantes <- c(adh_faltantes, c(2019, d, h))
      k <- k + 1
      
    }
  }
}


adh_faltantes <- matrix(adh_faltantes, nrow = k, byrow = TRUE)

# Grava csv com os valores faltantes

colnames(adh_faltantes) <- c("ano", "dia", "hora")
setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_prep/clima_ny_faltantes/")
write.csv(
  data.frame(adh_faltantes),
  file = paste0('MSV', "B_faltantes.csv"),
  row.names = FALSE
)

#-----------------------------------------------------------
# Verifica em que minuto as observações costumam chegar ----
#-----------------------------------------------------------
# Cria função para a moda

pegaModa <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda <- pegaModa(df_1518$minuto)

#-----------------------------------------------------------
# Imputação de valores faltantes ----
#-----------------------------------------------------------

df_1518$DATE <- as.character(df_1518$DATE)

imputacao <- data.frame(matrix(nrow = k, ncol = 6))

for (i in 1:(k)) {
  imputacao[i, 1] <- "imputado"
  imputacao[i, 2] <- 99999
  imputacao[i, 3] <- adh_faltantes[i, 1]
  imputacao[i, 4] <- adh_faltantes[i, 2]
  imputacao[i, 5] <- adh_faltantes[i, 3]
  imputacao[i, 6] <- moda
  imputacao[i, 7] <- 9999
  
}

names(imputacao) <- names(df_1518)

df_1518 <- bind_rows(df_1518, imputacao)

df_1518 <- df_1518[with(df_1518, order(ano, dia, hora)), ]

df_1518$TMP[df_1518$TMP == 99999] <- NA

df_1518$TMP <- na.interp(df_1518$TMP)

df_1518 <-
  as.data.table(df_1518)[, mean(TMP), by = .(ano, dia, hora)]

#-----------------------------------------------------------
# Passa para GMT-4 ----
#-----------------------------------------------------------
df_1518$TMP[1:(nrow(df_1518) - 4)] <- df_1518$TMP[5:nrow(df_1518)]
indice_ultimo <-
  which((df_1518$ano == 2018) &
          (df_1518$dia == 365) & (df_1518$hora == 23))
df_1518 <- df_1518[1:indice_ultimo, ]

df_1518$hora <- df_1518$hora + 1

df_final <- df_1518[, c("ano", "dia", "hora", "V1")]

names(df_final) <- c('Ano', 'Dia', 'Hora', 'Temperatura')

df_final$Temperatura <- as.numeric(df_final$Temperatura)

ts.plot(df_final$Temperatura)

#-----------------------------------------------------------
# Grava ----
#-----------------------------------------------------------

setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_prep/clima_ny_temps_sep")

write.csv(
  df_final,
  file = paste0("temps_ny_1518_", 'MSV', ".csv"),
  row.names = FALSE
)


# https://www.timeanddate.com/calendar/seasons.html?year=2000&n=12
# https://www.public-holidays.us/US_BG_2015_New%20York
# https://www.timeanddate.com/time/change/usa/new-york-state?year=2016
