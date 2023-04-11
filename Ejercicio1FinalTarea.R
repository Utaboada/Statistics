# Importamos el data frame desde un archivo csv:
library(readxl)
craneos <- read_excel("C:/Users/evaro/OneDrive/Escritorio/UCM/Estadística/Libro2.xlsx", 
                      col_types = c("numeric", "skip", "numeric"))
View(craneos)

# Instalamos paquetes que vamos a usar después:
install.packages("modeest")
install.packages("moments")
library(modeest)
library(moments)

# Renombramos las variables del data frame para que sea más fácil de realizar el estudio:
names(craneos)[1]<-paste("epoca")
names(craneos)[2]<-paste("altura")

# Observamos los primeros valores del data frame:
head(craneos)


# ----------------------------------------------------------------------------------------

# Dividimos el dataset en dos sub data frames:
# Uno con los datos de la época 1, es decir, predinástico temprano.
# Otro con los datos de la época 2, es decir, predinástico tardío.
craneos1 <- subset(craneos, epoca == 1)
craneos1

craneos2 <- subset(craneos, epoca == 2)
craneos2

#Veamos cuántos datos hay en cada uno de los data frame:
n1 <- length(craneos1$altura)
n2 <- length(craneos2$altura)


# ----------------------------------------------------------------------------------------

# Calculamos las medidas de centralización:

# Medidas de posición central:

# Media aritmética para cada época:
mediaCraneos1 <- mean(craneos1$altura)
mediaCraneos1

mediaCraneos2 <- mean(craneos2$altura)
mediaCraneos2

# Media geométrica para cada época;
geometric <- function(x){
  exp(sum(log(x))/length(x))
}

mediageomCraneos1 <- geometric(craneos1$altura)
mediageomCraneos1

mediageomCraneos2 <- geometric(craneos2$altura)
mediageomCraneos2

# Mediana para cada época:
medianaCraneos1 <- median(craneos1$altura)
medianaCraneos1

medianaCraneos2 <- median(craneos2$altura)                      
medianaCraneos2

# Moda para cada época (FORMA 1):
moda1Craneos1 <- mfv(craneos1$altura)
moda1Craneos1

moda1Craneos2 <- mfv(craneos2$altura)
moda1Craneos2

#Moda para cada época (FORMA 2):
moda2 = function(x){
  q <- unique(x)
  q[which.max(tabulate(match(x,q)))]
}

moda2Craneos1 <- moda2(craneos1$altura)
moda2Craneos1

moda2Craneos2 <- moda2(craneos2$altura)
moda2Craneos2

# Medidas de posición no centrales:

# Cuartiles para cada época:
cuartilesCraneos1 <- quantile(craneos1$altura)
cuartilesCraneos1

cuartilesCraneos2 <- quantile(craneos2$altura)
cuartilesCraneos2

# Deciles para cada época:
decilesCraneos1 <- quantile(craneos1$altura, prob = seq(0, 1, length = 11))
decilesCraneos1

decilesCraneos2 <- quantile(craneos2$altura, prob = seq(0, 1, length = 11))
decilesCraneos2

# Percentiles para cada época:
percentilesCraneos1 <- quantile(craneos1$altura, prob = seq(0, 1, length = 101))
percentilesCraneos1

percentilesCraneos2 <- quantile(craneos2$altura, prob = seq(0, 1, length = 101))
percentilesCraneos2


# ----------------------------------------------------------------------------------------

# Calculamos las medidas de dispersión:

# Rango para cada época:
rangoCraneos1 <- range(craneos1$altura)
rangoCraneos1

rangoCraneos2 <- range(craneos2$altura)
rangoCraneos2

# Varianza para cada época:
varCraneos1 <- var(craneos1$altura)
varCraneos1

varCraneos2 <- var(craneos2$altura)
varCraneos2

# Desviación típica para cada época: podemos calcularla con la funciÃ³n de r o 
# como la raíz cuadrada de la varianza.

sdCraneos1 <- sd(craneos1$altura)
sdCraneos1

sdCraneos2 <- sd(craneos2$altura)
sdCraneos2

# Coeficiente de variación de Pearson de cada época:
CV <- function(x){
  y <- 100*sd(x)/mean(x)
  return(y)
}

CVCraneos1 <- CV(craneos1$altura)
CVCraneos1

CVCraneos2 <- CV(craneos2$altura)
CVCraneos2


# ----------------------------------------------------------------------------------------

# Calculamos las medidas de forma:

# Sesgo de cada época: 
sesgoCraneos1 <- skewness(craneos1$altura)
sesgoCraneos1

sesgoCraneos2 <- skewness(craneos2$altura)
sesgoCraneos2

# Como el sesgo es negativo, podemos concluir que la distribución de estos datos
# es sesgada a la izquierda, es decir, presenta una distribución asimÃ©trica negativa
# (más valores a la izquierda de la media que a su derecha)

# Curtosis de cada época:
curtosisCraneos1 <- kurtosis(craneos1$altura)
curtosisCraneos1
# En este caso, como la curtosis es mayor que 3, podemos concluir que la distribución de los 
# datos es leptocúrtica: presenta un elevado grado de concentración alrededor de los valores 
# centrales de la variable.

curtosisCraneos2 <- kurtosis(craneos2$altura)
curtosisCraneos2
# En este caso, como la curtosis es menor que 3, podemos concluir que la distribución de los 
# datos es platicúrtica: presenta un reducido grado de concentración alrededor de los valores 
# centrales de la variable.


# ----------------------------------------------------------------------------------------

# Obtenemos el diagrama de caja y bigotes.
boxplot(craneos1$altura, xlab = "Época del predinástico temprano", ylab = "Altura del cráneo")
boxplot(craneos2$altura, xlab = "Época del predinástico tardío", ylab = "Altura del cráneo")
boxplot(craneos$altura, xlab = "Época del predinástico", ylab = "Altura del cráneo")


# ----------------------------------------------------------------------------------------

# Determinar si cada una de las dos sub-muestras sigue una distribución normal utilizando el test de Kolmogorov-Smirnov
ks.test(craneos1$altura, pnorm, mean = mediaCraneos1, sd = sdCraneos1)


ks.test(craneos2$altura, pnorm, mean = mediaCraneos2, sd = sdCraneos2)


###################### Se puede adjuntar un histograma para ver la distribución de los datos y ver que representa una normal:
#hist(craneos1$altura)
#hist(craneos2$altura)
