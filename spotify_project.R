  ## -----------------------------------
## Estatistica Computacional (EST1008)
## FAEST - ICEN - UFPA
## Prof. Dr. rer. nat. Marcelo Protazio
## ------------------------------------
  
library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)

## ----------------------
## [1] Ler Banco de Dados
## ----------------------

data = read.csv('spotify_top10_dataset.csv',sep=',',na.strings="NA", header=T)

## -----------------------------------
##class(data)
##
##typeof(data)
##
##head(data)
##
##colnames(data)
##
##ncol(data)
##
##nrow(data)
##
## -----------------------------------

## ----------------------
## [2] Estatística Descritiva
## ----------------------

# Resumo dos dados de cada coluna
summary(data)

# Calcula o desvio padrão da coluna dnce
sapply(data["dnce"], sd)

# Calcula o desvio padrão da coluna spch
sapply(data["spch"], sd) # desvio padrão alto

# Plota o gráfico de barras de músicas por ano
barplot(table(data["year"]), col="blue", las=2)

# Pega os 10 gêneros mais tocados
top_genres = as.data.frame(table(data["top.genre"]))
top_genres = top_genres %>% arrange(desc(top_genres$Freq))
top_genres = top_genres[1:10, ]

# Ajuste de margens
par(mar=c(7,5,4,1)+.1)

# Plota o gráfico de barras dos 10 gêneros mais tocados
barplot(top_genres[, "Freq"], names.arg = list("dance pop", "pop", 
                                               "canadian pop", "barbadian pop",
                                               "boy band", "electropop",
                                               "british soul", "big room",
                                               "canadian r&b",
                                               "neo mellow"), las=2, 
        horiz = TRUE, main = "10 Gêneros Mais Tocados Entre 2010 e 2019"
        , xlab = "Frequência", xlim = c(0,350))

# Pega os 5 artistas com mais hits
top_artists = as.data.frame(table(data["artist"]))
top_artists = top_artists %>% arrange(desc(top_artists$Freq))
top_artists = top_artists[1:5, ]
top_artists

# Ajuste de margens
par(mar=c(7,5,4,1)+.1)

# Plota o gráfico de barrras dos 5 artistas com mais hits
barplot(top_artists[, "Freq"], names.arg = list("Katy Perry", "Justin Bieber",
                                               "Maroon 5", "Rihanna",
                                               "Lady Gaga"), 
        las=2, main = "5 Artistas Com Mais Hits Entre 2010 e 2019",
        ylab = "Músicas", ylim = c(0,20), width = c(1.5, 1.5, 1.5 , 1.5, 1.5))

# Guarda quantas hits por ano a Katy Perry teve
bar_kp = data %>% filter(artist == "Katy Perry")
bar_kp = as.data.frame(table(bar_kp["year"]))
bar_kp$Var1 <- as.numeric(as.character(bar_kp$Var1))
bar_kp

# Plota histograma da variável acima
barplot(bar_kp$Freq, ylab="Músicas", xlab="Ano", main="Músicas por ano da Katy Perry",
        names.arg=c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))

# Calcula a média de val para cada ano
val_mean = data %>% group_by(year) %>% summarise(me = mean(val))

# Plota as médias acima
barplot(val_mean$year, height = val_mean$me, ylab = "Média de val", xlab = "Ano",
        main = "Média de 'val' Por Ano", names.arg = val_mean$year, ylim = c(0,70))

# Calcula a média de dur para cada ano
spch_mean = data %>% group_by(year) %>% summarise(me = mean(spch))

# Plota as médias acima
barplot(spch_mean$year, height = spch_mean$me, ylab = "Média de spch", xlab = "Ano",
main = "Média de 'spch' Por Ano", names.arg = spch_mean$year)
