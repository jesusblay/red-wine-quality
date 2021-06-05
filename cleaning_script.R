# Cargamos los paquetes de R que vamos a usar
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(relaimpo)
library (effects)
library(lmtest)

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


# Estudio de la correlción
correlation <- cor(wine)
corrplot(correlation, type ="upper", tl.col="black")



# Estadísticas de valores vacíos
colSums(is.na(wine))
colSums(wine=='')

# Estadísticas de valores cero
colSums(wine==0)

# Verificamos la dimensión y la estructura del conjunto de datos 
ggplot(data=wine, aes(x=quality)) + geom_bar()

# Vamos a revisar la distribución de las variables mediante un histograma para ver si a priori detectamos outliers
# Para ello vamos primero a escalar nuestros datos 

wine_scaled <- data.frame(scale(wine))
multi.hist(x = wine_scaled, dcol = c("blue", "red"), dlty = c("dotted", "solid"), 
           main = "")


# Vamos a analizar la correlación de nuestras variables
corrplot(correlation, type ="upper", tl.col="black", method = 'number')



# Realizamos nuestro modelo excluyendo PH - FREE.SULFUR.DIOXIDE - CITRIC.ACID

mlm <- lm(quality ~ alcohol + sulphates + volatile.acidity + fixed.acidity + total.sulfur.dioxide
          + density + chlorides + residual.sugar,  data = wine )

summary(mlm)

#Realizamos Step Wise para ver si vale la pena quitar alguna variable:

Step_wise <- step(mlm, direction = "both", trace = 1)

# Modelo de regresión lineal con datos escalados:

mlm_scaled <- lm(quality ~ alcohol + sulphates + volatile.acidity + fixed.acidity + total.sulfur.dioxide
          + density + chlorides + residual.sugar,  data = wine_scaled )
summary(mlm_scaled)

# Importancia de las variablas independientes

calc.relimp(mlm, type = c("lmg"), rela = TRUE, rank = TRUE)

# Colinealidad

require(car)
vif(mlm)

# Análisis de la normalidad de los residuos:

plot1 <- ggplot(data = wine, aes(alcohol, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = wine, aes(sulphates, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

plot3 <- ggplot(data = wine, aes(volatile.acidity, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

plot4 <- ggplot(data = wine, aes(fixed.acidity, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

plot5 <- ggplot(data = wine, aes(total.sulfur.dioxide, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

plot6 <- ggplot(data = wine, aes(density, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

plot7 <- ggplot(data = wine, aes(chlorides, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

plot8 <- ggplot(data = wine, aes(residual.sugar, mlm$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6,plot7, plot8)

qqnorm(mlm$residuals)
qqline(mlm$residuals)

shapiro.test(mlm$residuals)

# Test de Breusch-Pagan para la heteroscedasticidad

bptest(mlm)

