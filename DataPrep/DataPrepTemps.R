# Tese - Códigos para preparação dos dados de temperatura --------------------------------------------
# Para estações exceto MSV.

# Obs.: recomendamos executar bloco a bloco.

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------
library(readr)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(forecast)

#--------------------------------------------------
# Muda diretório de trabalho ----
#--------------------------------------------------

setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_originais/")

#--------------------------------------------------
# Pega nome das estações ----
#--------------------------------------------------

estacoes <- list.dirs()[-1]
estacoes <- sub(".", "", estacoes)
estacoes <- sub("/", "", estacoes)

# Exclui MSV, cujo comportamento é diferente.

estacoes <- estacoes[estacoes != "MSV"]

for (t in estacoes) {
  rm(list = ls()[!(ls() %in% c('t', 'estacoes'))])
  
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
  # Converte temperaturas para numérico e corrige escala ----
  #-----------------------------------------------------------
  
  df_1518$TMP <- str_replace(df_1518$TMP, ",", ".")
  
  df_1518$TMP <- str_replace(df_1518$TMP, c("A|C|I|M|P|R|U"), "0")
  
  df_1518$TMP <- as.numeric(df_1518$TMP)
  
  df_1518$TMP <- df_1518$TMP / 10
  
  #-----------------------------------------------------------
  # Inclui colunas para  ano, dia juliano, hora e minuto ----
  #-----------------------------------------------------------
  df_1518$ano <- year(df_1518$DATE)
  df_1518$dia <- yday(df_1518$DATE)
  df_1518$hora <- hour(df_1518$DATE)
  df_1518$minuto <- minute(df_1518$DATE)
  
  df_1518 <- na.omit(df_1518)
  
  #-----------------------------------------------------------
  # Verifica em que minuto as observações costumam chegar ----
  #-----------------------------------------------------------
  # Cria função para a moda
  
  pegaModa <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  moda <- pegaModa(df_1518$minuto)
  
  # Cria flag para encontrar os registros fora do instante esperado

  df_1518$flag_minuto <- rep(0, times = nrow(df_1518))
  
  for (i in 1:nrow(df_1518)) {
    if (df_1518$minuto[i] != moda) {
      df_1518$flag_minuto[i] <- 1
      
    }
    
  }

  # Remove as observações fora do instante esperado ----
  df_1518 <- df_1518 %>% filter(flag_minuto == 0)
  
  #---------------------------------------
  # Verifica se há valores faltantes ----
  #---------------------------------------
  # ano, dia e hora dos valores sem registro (linhas faltantes)
  
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

  # Último dia do ano bissexto de 2016
  for (h in 0:23) {
    teste <-
      which((df_1518$dia == 366) &
              (df_1518$hora == h) & (df_1518$ano == 2016))
    
    if (length(teste) == 0) {
      adh_faltantes <- c(adh_faltantes, c(2016, 366, h))
      k <- k + 1
      
    }
    
  }
  
  # ano de 2019
  for (d in 1:135) {
    for (h in 0:23) {
      teste <-
        which((df_1518$dia == d) &
                (df_1518$hora == h) & (df_1518$ano == 2019))
      
      if (length(teste) == 0) {
        adh_faltantes <- c(adh_faltantes, c(2019, d, h))
        k <- k + 1
        
      }
    }
  }
  
  max_t = max(df_1518$TMP)
  
  r <- 0
  
  # Checa NAs (999.9)
  if (max_t > 900) {
    for (i in 1:nrow(df_1518)) {
      if (df_1518$TMP[i] == max_t) {
        adh_faltantes <-
          c(adh_faltantes,
            c(df_1518$ano[i], df_1518$dia[i], df_1518$hora[i]))
        r <- r + 1
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
    file = paste0(t, "_faltantes.csv"),
    row.names = FALSE
  )
  
  #-----------------------------------------------------------
  # Checa duplicatas ----
  #-----------------------------------------------------------
  n_duplicatas <- sum(duplicated(df_1518$DATE))
  
  df_1518    <- df_1518[!duplicated(df_1518$DATE),]
  
  #-----------------------------------------------------------
  # Imputação de valores faltantes ----
  #-----------------------------------------------------------
  
  df_1518$DATE <- as.character(df_1518$DATE)
  
  imputacao <- data.frame(matrix(nrow = k - r, ncol = 7))
  
  for (i in 1:(k - r)) {
    imputacao[i, 1] <- "imputado"
    imputacao[i, 2] <- 99999
    imputacao[i, 3] <- adh_faltantes[i, 1]
    imputacao[i, 4] <- adh_faltantes[i, 2]
    imputacao[i, 5] <- adh_faltantes[i, 3]
    imputacao[i, 6] <- moda
    imputacao[i, 7] <- 0
    
  }
  
  names(imputacao) <- names(df_1518)
  
  df_1518 <- bind_rows(df_1518, imputacao)
  
  df_1518 <- df_1518[with(df_1518, order(ano, dia, hora)), ]
  
  df_1518$TMP[df_1518$TMP == 99999] <- NA
  
  if (max_t > 900) {
    
    df_1518$TMP[df_1518$TMP == max_t] <- NA
    
  }
  
  df_1518$TMP <- na.interp(df_1518$TMP)
  
  #-----------------------------------------------------------
  # Passa para GMT-4 ----
  #-----------------------------------------------------------
  df_1518$TMP[1:(nrow(df_1518) - 4)] <-
    df_1518$TMP[5:nrow(df_1518)]
  indice_ultimo <-
    which((df_1518$ano == 2018) &
            (df_1518$dia == 365) & (df_1518$hora == 23))
  df_1518 <- df_1518[1:indice_ultimo, ]
  
  df_1518$hora <- df_1518$hora + 1
  
  df_final <- df_1518[, c("ano", "dia", "hora", "TMP")]
  
  names(df_final) <- c("Ano", "Dia", "Hora", "Temperatura")
  
  df_final$Temperatura <- as.numeric(df_final$Temperatura)
  
  ts.plot(df_final$Temperatura)
  
  #-----------------------------------------------------------
  # Grava ----
  #-----------------------------------------------------------
  setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_prep/clima_ny_temps_sep")
  
  write.csv(df_final,
            file = paste0("temps_ny_1518_", t, ".csv"),
            row.names = FALSE)
  
}

# https://www.timeanddate.com/calendar/seasons.html?year=2000&n=12
# https://www.public-holidays.us/US_BG_2015_New%20York
# https://www.timeanddate.com/time/change/usa/new-york-state?year=2016
