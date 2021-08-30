setwd("C:/Users/Pc/Documents/PGMC 31-07-2020 on/Tese/Cap2")

library(readr)
library(ggplot2)
library(cowplot)
library(showtext)
library(scales)
library("tidyverse")
library("TTR")

df_loads <- read_delim("loadsny1518.csv", ",")
df_loads <- df_loads[1:35040,]

dfl <- aggregate(cbind(WEST, GENESE, CENTRL, NORTH, MHKVL, CAPITL) ~ day + season, data = df_loads, FUN = mean, na.rm = TRUE)
# there are days in which there are two seasons

df_temps <- read_delim("tempsny1518.csv", ",")
df_temps <- df_temps[1:35040,]

df_temps$season <- df_loads$season
df_temps$day <- df_loads$day

dft <- aggregate(cbind(ALB, ART, BGM, BUF, HPN, JFK, LGA, MSS, RME, ROC, SYR) ~ day + season, data = df_temps, FUN = mean, na.rm = TRUE)
# there are days in which there are two seasons

dflt <- cbind(dfl, dft[,3:13])

library("plyr")
dflt$season <- factor(dflt$season)
dflt$season <- revalue(dflt$season, c("1" = "Primavera", "2"="Verão", "3" = "Outono", "4"="Inverno"))

dflt <- filter(dflt, season == "Primavera")

dflt <- dflt[sample(nrow(dflt), 30), ]

write.csv(dflt, "dflt.csv")

ggplot(dflt, aes(x = BUF, y = WEST)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)") +
  geom_point(alpha = 0.8, size = 2.5) + 
  theme(axis.title.x = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.title.y = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.text.x = element_text(size=18,family="lm10")) +
  theme(axis.text.y = element_text(size=18, family="lm10")) +
  theme(legend.text = element_text(size=18, family="lm10")) +
  theme(legend.title = element_text(size=0, family="lm10")) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(panel.background = element_blank()) + 
  theme(axis.line = element_line(size = 1)) +
  theme(legend.key = element_blank())

font_add("lm10", regular = "lmroman10-regular.otf",
         bold = "lmroman10-Bold.otf")
showtext_auto()

model <- lm(WEST~BUF, data = dflt)

dflt$line <- predict(model, data.frame(dflt))

ggplot(dflt, aes(x = BUF, y = WEST)) +
  geom_line(aes(x = BUF, y = line)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)") +
  geom_point(alpha = 0.8, size = 2.5) + 
  geom_segment(aes(xend= BUF,yend=line), linetype = "dashed" , show.legend=T) +
  theme(axis.title.x = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.title.y = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.text.x = element_text(size=18,family="lm10")) +
  theme(axis.text.y = element_text(size=18, family="lm10")) +
  theme(legend.text = element_text(size=18, family="lm10")) +
  theme(legend.title = element_text(size=0, family="lm10")) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(panel.background = element_blank()) + 
  theme(axis.line = element_line(size = 1)) +
  theme(legend.key = element_blank())

model2 <- lm(WEST ~ I(BUF^1) + I(BUF^2) + I(BUF^3) + I(BUF^4) + I(BUF^5) + I(BUF^6) +I(BUF^7), data = dflt)

dflt$poly <- predict(model2,data.frame(dflt))

ggplot(dflt, aes(x = BUF, y = WEST, color = variable)) +
  geom_line(aes(x = BUF, y = poly)) +
  labs(x = "Temperatura média diária (ºC)", y = "Carga média diária (MW)") +
  geom_point(alpha = 0.8, size = 2.5) + 
  geom_segment(aes(xend= BUF,yend=poly), linetype = "dashed" , show.legend=T) +
  theme(axis.title.x = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.title.y = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.text.x = element_text(size=18,family="lm10")) +
  theme(axis.text.y = element_text(size=18, family="lm10")) +
  theme(legend.text = element_text(size=18, family="lm10")) +
  theme(legend.title = element_text(size=0, family="lm10")) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(panel.background = element_blank()) + 
  theme(axis.line = element_line(size = 1)) +
  


library(ggplot2)
library(showtext)



ggplot(data = d, aes(x = , y = y)) +
  geom_point(color = "#003f5c", size = 3) +
  geom_line(aes(x = X, y = poly)) +
  #geom_segment(aes(xend=X,yend=line), linetype = "dashed" , show.legend=T) +
  theme(axis.title.x = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.title.y = element_text(size=18, family="lm10", face = 'bold')) +
  theme(axis.text.x = element_text(size=0,family="lm10")) +
  theme(axis.text.y = element_text(size=18, family="lm10")) +
  theme(legend.text = element_text(size=18, family="lm10")) +
  theme(legend.title = element_text(size=0, family="lm10")) + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  theme(axis.ticks.x = element_blank()) +
  theme(panel.background = element_blank()) + 
  theme(axis.line = element_line(size = 1))