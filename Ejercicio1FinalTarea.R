# Importamos el data frame desde un archivo csv:
library(readxl)
craneos <- read_excel("C:/Users/evaro/OneDrive/Escritorio/UCM/Estad�stica/Libro2.xlsx", 
                      col_types = c("numeric", "skip", "numeric"))
View(craneos)

# Instalamos paquetes que vamos a usar despu�s:
install.packages("modeest")
install.packages("moments")
library(modeest)
library(moments)

# Renombramos las variables del data frame para que sea m�s f�cil de realizar el estudio:
names(craneos)[1]<-paste("epoca")
names(craneos)[2]<-paste("altura")

# Observamos los primeros valores del data frame:
head(craneos)


# ----------------------------------------------------------------------------------------

# Dividimos el dataset en dos sub data frames:
# Uno con los datos de la �poca 1, es decir, predin�stico temprano.
# Otro con los datos de la �poca 2, es decir, predin�stico tard�o.
craneos1 <- subset(craneos, epoca == 1)
craneos1

craneos2 <- subset(craneos, epoca == 2)
craneos2

#Veamos cu�ntos datos hay en cada uno de los data frame:
n1 <- length(craneos1$altura)
n2 <- length(craneos2$altura)


# ----------------------------------------------------------------------------------------

# Calculamos las medidas de centralizaci�n:

# Medidas de posici�n central:

# Media aritm�tica para cada �poca:
mediaCraneos1 <- mean(craneos1$altura)
mediaCraneos1

mediaCraneos2 <- mean(craneos2$altura)
mediaCraneos2

# Media geom�trica para cada �poca;
geometric <- function(x){
  exp(sum(log(x))/length(x))
}

mediageomCraneos1 <- geometric(craneos1$altura)
mediageomCraneos1

mediageomCraneos2 <- geometric(craneos2$altura)
mediageomCraneos2

# Mediana para cada �poca:
medianaCraneos1 <- median(craneos1$altura)
medianaCraneos1

medianaCraneos2 <- median(craneos2$altura)                      
medianaCraneos2

# Moda para cada �poca (FORMA 1):
moda1Craneos1 <- mfv(craneos1$altura)
moda1Craneos1

moda1Craneos2 <- mfv(craneos2$altura)
moda1Craneos2

#Moda para cada �poca (FORMA 2):
moda2 = function(x){
  q <- unique(x)
  q[which.max(tabulate(match(x,q)))]
}

moda2Craneos1 <- moda2(craneos1$altura)
moda2Craneos1

moda2Craneos2 <- moda2(craneos2$altura)
moda2Craneos2

# Medidas de posici�n no centrales:

# Cuartiles para cada �poca:
cuartilesCraneos1 <- quantile(craneos1$altura)
cuartilesCraneos1

cuartilesCraneos2 <- quantile(craneos2$altura)
cuartilesCraneos2

# Deciles para cada �poca:
decilesCraneos1 <- quantile(craneos1$altura, prob = seq(0, 1, length = 11))
decilesCraneos1

decilesCraneos2 <- quantile(craneos2$altura, prob = seq(0, 1, length = 11))
decilesCraneos2

# Percentiles para cada �poca:
percentilesCraneos1 <- quantile(craneos1$altura, prob = seq(0, 1, length = 101))
percentilesCraneos1

percentilesCraneos2 <- quantile(craneos2$altura, prob = seq(0, 1, length = 101))
percentilesCraneos2


# ----------------------------------------------------------------------------------------

# Calculamos las medidas de dispersi�n:

# Rango para cada �poca:
rangoCraneos1 <- range(craneos1$altura)
rangoCraneos1

rangoCraneos2 <- range(craneos2$altura)
rangoCraneos2

# Varianza para cada �poca:
varCraneos1 <- var(craneos1$altura)
varCraneos1

varCraneos2 <- var(craneos2$altura)
varCraneos2

# Desviaci�n t�pica para cada �poca: podemos calcularla con la función de r o 
# como la ra�z cuadrada de la varianza.

sdCraneos1 <- sd(craneos1$altura)
sdCraneos1

sdCraneos2 <- sd(craneos2$altura)
sdCraneos2

# Coeficiente de variaci�n de Pearson de cada �poca:
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

# Sesgo de cada �poca: 
sesgoCraneos1 <- skewness(craneos1$altura)
sesgoCraneos1

sesgoCraneos2 <- skewness(craneos2$altura)
sesgoCraneos2

# Como el sesgo es negativo, podemos concluir que la distribuci�n de estos datos
# es sesgada a la izquierda, es decir, presenta una distribuci�n asimétrica negativa
# (m�s valores a la izquierda de la media que a su derecha)

# Curtosis de cada �poca:
curtosisCraneos1 <- kurtosis(craneos1$altura)
curtosisCraneos1
# En este caso, como la curtosis es mayor que 3, podemos concluir que la distribuci�n de los 
# datos es leptoc�rtica: presenta un elevado grado de concentraci�n alrededor de los valores 
# centrales de la variable.

curtosisCraneos2 <- kurtosis(craneos2$altura)
curtosisCraneos2
# En este caso, como la curtosis es menor que 3, podemos concluir que la distribuci�n de los 
# datos es platic�rtica: presenta un reducido grado de concentraci�n alrededor de los valores 
# centrales de la variable.


# ----------------------------------------------------------------------------------------

# Obtenemos el diagrama de caja y bigotes.
boxplot(craneos1$altura, xlab = "�poca del predin�stico temprano", ylab = "Altura del cr�neo")
boxplot(craneos2$altura, xlab = "�poca del predin�stico tard�o", ylab = "Altura del cr�neo")
boxplot(craneos$altura, xlab = "�poca del predin�stico", ylab = "Altura del cr�neo")


# ----------------------------------------------------------------------------------------

# Determinar si cada una de las dos sub-muestras sigue una distribuci�n normal utilizando el test de Kolmogorov-Smirnov
ks.test(craneos1$altura, pnorm, mean = mediaCraneos1, sd = sdCraneos1)


ks.test(craneos2$altura, pnorm, mean = mediaCraneos2, sd = sdCraneos2)


###################### Se puede adjuntar un histograma para ver la distribuci�n de los datos y ver que representa una normal:
#hist(craneos1$altura)
#hist(craneos2$altura)
