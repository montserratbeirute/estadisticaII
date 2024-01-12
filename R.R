#---Paquetes--------------------------------------------------------

library(stringr) # para cambiar todas las commas a puntos en la columna Ultimo Salario
library(ggplot2)
library(univariateML)
library(rriskDistributions)
library(boot)
library(ks)

#---CSV y formatos--------------------------------------------------------

# Abrir y dar formato a la base de datos

base_salarios <- read.csv("BaseSalarios.csv",  sep = ";")

base_salarios$Fec.Nac <- as.Date(base_salarios$Fec.Nac, format = "%d/%m/%Y")

colnames(base_salarios)[5] ="Ultimo.Salario"

base_salarios <- subset(base_salarios, select = -X)

base_salarios$Ultimo.Salario <- as.numeric(str_replace_all(base_salarios$Ultimo.Salario, ",", "."))

#---Parte I----------------------------------------------------

#Hombres
summary(base_salarios$Coutas[base_salarios$Sexo == 1])
summary(base_salarios$Ultimo.Salario[base_salarios$Sexo == 1])

#Mujeres
summary(base_salarios$Coutas[base_salarios$Sexo == 2])
summary(base_salarios$Ultimo.Salario[base_salarios$Sexo == 2])

#Boxplot
boxplot(Ultimo.Salario ~ Sexo, data = base_salarios, col = c("lightblue", "lightgreen"), main = "Boxplot de salario por sexo",
        xlab = "Categoría", ylab = "Valor")

#Comparacion de medias
t.test(
  x           = base_salarios$Ultimo.Salario[base_salarios$Sexo == 1],
  y           = base_salarios$Ultimo.Salario[base_salarios$Sexo == 2],
  paired      = FALSE,
  alternative = "two.sided",
  conf.level  = 0.95
)

#---Parte II----------------------------------------------------
#Histograma salarios
ggplot(base_salarios, aes(x= Ultimo.Salario)) +
  geom_histogram(binwidth = 150000, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma Simple", x = "Valor", y = "Frecuencia")

#Kernels
kernels <- eval(formals(density.default)$kernel)[1:6]
hist(base_salarios$Ultimo.Salario, breaks = 100, main= "Distribucion Salarios por Kernels", freq=FALSE)
lines(density(base_salarios$Ultimo.Salario ,kernel=kernels[1]), col = 1,lwd=2,lty=1)
lines(density(base_salarios$Ultimo.Salario ,kernel=kernels[2]), col = 2,lwd=2,lty=1)
lines(density(base_salarios$Ultimo.Salario ,kernel=kernels[3]), col = 3,lwd=2,lty=1)
lines(density(base_salarios$Ultimo.Salario ,kernel=kernels[4]), col = 4,lwd=2,lty=1)
lines(density(base_salarios$Ultimo.Salario ,kernel=kernels[5]), col = 5,lwd=2,lty=1)
lines(density(base_salarios$Ultimo.Salario ,kernel=kernels[6]), col = 6,lwd=2,lty=1)
legend("topright", legend = kernels, col = seq(kernels), lty = 1,lwd = 2,cex = 0.5)

#---Parte III----------------------------------------------------

#Criterio de informacion Akaike
model_select(base_salarios$Ultimo.Salario)

fit.cont(base_salarios$Ultimo.Salario)

#intervalo bootsrap

?bootstrapml

resultado.media <- boot(data = base_salarios$Ultimo.Salario, statistic = function(data,indices) mean(data[indices]), R = 1000)
mean(resultado.media$t)
resultado.sd <- boot(data = base_salarios$Ultimo.Salario, statistic = function(data,indices) sd(data[indices]), R = 1000)
mean(resultado.sd$t)

#---Parte IV----------------------------------------------------
densidad_kde <- kde(base_salarios$Ultimo.Salario)
plot(densidad_kde, main = "Estimación de Densidad de Kernel para Salarios", xlab = "Salarios", ylab = "Densidad")


#intervalo boostrap
resultado.media <- boot(data = base_salarios$Ultimo.Salario, statistic = function(data,indices) mean(data[indices]), R = 1000)
mean(resultado.media$t)
mean(resultado.media$t0)

plot(resultado.media)
