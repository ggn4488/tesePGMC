# Tese - Códigos para preparação dos dados de temperatura --------------------------------------------
# Consolidação dos dados.

#--------------------------------------------------
# Carrega bibliotecas ----
#--------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)

#--------------------------------------------------
# Muda diretório de trabalho ----
#--------------------------------------------------
setwd("~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_prep/clima_ny_temps_sep")

#--------------------------------------------------
# Cria data frame tidy ----
#--------------------------------------------------
df_tidy <-
  list.files(path = "~/PGMC 31-07-2020 on/Tese/Código/DataPrep/clima_ny_prep/clima_ny_temps_sep",
             pattern = "*.csv",
             full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows

estacao <- c()

for (t in 1:length(estacoes)) {
  estacao <-
    c(estacao, rep(estacoes[t], times = nrow(df_tidy) / length(estacoes)))
  
}

df_tidy$estacao <- estacao

#--------------------------------------------------
# Cria data frame untidy ----
#--------------------------------------------------

df_untidy <-
  df_tidy %>% pivot_wider(names_from = estacao, values_from = Temperatura)

#--------------------------------------------------
# Grava ----
#--------------------------------------------------

setwd("~/PGMC 31-07-2020 on/Tese/Código/Cap2/")

write.csv(df_tidy, "temps_ny_1518_tidy.csv", row.names = FALSE)
write.csv(df_untidy, "temps_ny_1518_untidy.csv", row.names = FALSE)