# Cargamos los paquetes de R que vamos a usar
library(ggplot2)
library(dplyr)
library(gridExtra)

# Cargamos el fichero de datos
wine <- read.csv('winequality-red.csv', header=TRUE)

# Verificamos la dimensión y la estructura del conjunto de datos 
dim(wine)
summary(wine)


# Distribución de fixed.acidity, volatile.acidity, citric.acid y residual.sugar en función de la calidad del vino      
v1 <- ggplot(data=wine, aes(x=factor(quality), y=fixed.acidity, color=quality)) +   geom_boxplot()
v2 <- ggplot(data=wine, aes(x=factor(quality), y=volatile.acidity, color=quality)) + geom_boxplot()
v3 <- ggplot(data=wine, aes(x=factor(quality), y=citric.acid, color=quality)) + geom_boxplot()
v4 <- ggplot(data=wine, aes(x=factor(quality), y=residual.sugar, color=quality)) + geom_boxplot()

grid.arrange(v1, v2, v3, v4, ncol = 2, nrow = 2)

# Distribución de chlorides, free.sulfur.dioxide, total.sulfur.dioxide y density en función de la calidad del vino               
v5 <- ggplot(data=wine, aes(x=factor(quality), y=chlorides, color=quality)) + geom_boxplot()
v6 <- ggplot(data=wine, aes(x=factor(quality), y=free.sulfur.dioxide, color=quality)) + geom_boxplot()
v7 <- ggplot(data=wine, aes(x=factor(quality), y=total.sulfur.dioxide, color=quality)) + geom_boxplot()
v8 <- ggplot(data=wine, aes(x=factor(quality), y=density, color=quality)) + geom_boxplot()

grid.arrange(v5, v6, v7, v8, ncol = 2, nrow = 2)

# Distribución de pH, sulphates, y alcohol en función de la calidad del vino                                      
v9 <- ggplot(data=wine, aes(x=factor(quality), y=pH, color=quality)) + geom_boxplot()
v10 <- ggplot(data=wine, aes(x=factor(quality), y=sulphates, color=quality)) + geom_boxplot()
v11 <- ggplot(data=wine, aes(x=factor(quality), y=alcohol, color=quality)) + geom_boxplot()

grid.arrange(v9, v10, v11, ncol = 2, nrow = 2)

# Estadísticas de valores vacíos
colSums(is.na(wine))
colSums(wine=='')

# Estadísticas de valores cero
colSums(wine==0)

# Verificamos la dimensión y la estructura del conjunto de datos 
ggplot(data=wine, aes(x=quality)) + geom_bar()

