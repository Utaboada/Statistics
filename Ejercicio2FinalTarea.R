# Media de los cráneos en cada periodo:
mediaCraneos1 <- mean(craneos1$altura)
mediaCraneos1

mediaCraneos2 <- mean(craneos2$altura)
mediaCraneos2

# Las condiciones que tienen que cumplir las muestras para realizar el t.test son:
# Ser muestras aleatorias, independientes y distribuciones normales con varianzas desconocidas pero iguales.

# Estamos suponiendo que son independientes por enunciado del ejercicio, siguen distribuciones normales porque lo hemos concluido en el apartado
# anterior y son muestras aleatorias.

#El siguiente comando sólo nos da el intervalo de confianza
t.test(craneos1$altura, craneos2$altura, paires = TRUE)$conf.int

# Con este comando obtenemos tanto el test como el intervalo de confianza.
t.test(craneos1$altura, craneos2$altura, paired = TRUE, conf.level = 0.90)
t.test(craneos1$altura, craneos2$altura, paired = TRUE, conf.level = 0.95)
t.test(craneos1$altura, craneos2$altura, paired = TRUE, conf.level = 0.99)

# En los tres casos vamos a rechazar la hipótesis nula de que las medias sean iguales pues tenemos un p-value = 3.296e-06, por lo tanto, menor en cualquier caso
# que el nivel de significación, que en nuestros casos es alpha = 0.1, alpha = 0.05, alpha = 0.01.

# Los intervalos de confianza que obtenemos son:
# Nivel de confianza 0.9: (1.05565, 1.94435)
# Nivel de confianza 0.95: (0.965139, 2.034861)
# Nivel de confianza 0.99: (0.77916, 2.22084)



#install.packages("ggplot2")
#library(ggplot2)
#ggplot(craneos$altura)
