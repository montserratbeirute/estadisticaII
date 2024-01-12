#---Paquetes--------------------------------------------------------

library(stringr) # para cambiar todas las commas a puntos en la columna Ultimo Salario
library(ggplot2) # para hacer graficos
library(cowplot) # facilita creación de gráficos
library(univariateML) # para realizar el análisis AIC
library(rriskDistributions) # para realizar el análisis AIC

#---CSV y formatos--------------------------------------------------------

# Abrir y dar formato a la base de datos

base_salarios <- read.csv("BaseSalarios.csv",  sep = ";")

base_salarios$Fec.Nac <- as.Date(base_salarios$Fec.Nac, format = "%d/%m/%Y")

colnames(base_salarios)[5] ="Ultimo.Salario"

base_salarios <- subset(base_salarios, select = -X)

base_salarios$Ultimo.Salario <- as.numeric(str_replace_all(base_salarios$Ultimo.Salario, ",", "."))

options(scipen = 999) # para que R imprima los números normales

#----Parte I---------------------------------------------------

# Creamos dos bases de datos con la info de cuotas y ultimo salario para cada sexo
datos_hombres <- subset(base_salarios,Sexo == '1',select = c('Coutas','Ultimo.Salario'))
datos_mujeres <- subset(base_salarios,Sexo == '2',select = c('Coutas','Ultimo.Salario'))

nrow(datos_hombres) # 32723 hombres
nrow(datos_mujeres) # 73279 mujeres

# Obtenemos el valor maximo, minimo, media y varianza para cada una de las bases de datos para la columna cuotas y ultimo salario
analisis_hombres <- sapply(datos_hombres, function(x) c(max=max(x), min=min(x), mean = mean(x), var=var(x)))
analisis_mujeres <- sapply(datos_mujeres, function(x) c(max=max(x), min=min(x), mean = mean(x), var=var(x)))



# Realizamos un gráfico plotbox para el último salario reportado

grf_1 <- ggplot(base_salarios, aes(x=as.factor(Sexo), y=Ultimo.Salario)) + geom_boxplot(fill = c("lightskyblue4", "lightskyblue2"))  + cowplot::theme_cowplot() + scale_y_continuous(
    breaks = seq(min(base_salarios$Ultimo.Salario), max(base_salarios$Ultimo.Salario), length.out = 10), 
    labels = scales::comma_format(scale = 1e-3)) + labs(x= "Sexo", y="Ultimo salario reportado") +
    scale_x_discrete(labels = c("Hombre", "Mujer")) 

print(grf_1)


# Llevamos a cabo una prueba de hipotesis para ver si las medias del salario para hombres y mujeres son iguales

t.test(datos_hombres$Ultimo.Salario,datos_mujeres$Ultimo.Salario, alternative = "greater")

# EXTRA: eliminamos los dos salarios más altos de los hombres para hacer otro análisis

# base_datos_sin_salarios_altos <- base_salarios[-c(84807, 96368), ]

grf_2 <- ggplot(base_datos_sin_salarios_altos, aes(x=as.factor(Sexo), y=Ultimo.Salario)) + geom_boxplot(fill = c("lightskyblue4", "lightskyblue2"))  + cowplot::theme_cowplot() + scale_y_continuous(
  breaks = seq(min(base_datos_sin_salarios_altos$Ultimo.Salario), max(base_datos_sin_salarios_altos$Ultimo.Salario), length.out = 10), 
  labels = scales::comma_format(scale = 1e-3)) + labs(x= "Sexo", y="Ultimo salario reportado") +
  scale_x_discrete(labels = c("Hombre", "Mujer")) 

print(grf_2) 

#----Parte II---------------------------------------------------

# Creamos un histograma con los salarios
ult.salario <- base_salarios$Ultimo.Salario
media_salarios <- mean(ult.salario)

hist(ult.salario,
     main = "Histograma de últimos salarios",
     xlab = "Salarios",
     ylab = "Frecuencia",
     col = "lightskyblue2", breaks=20)

abline(v = media_salarios, col = "red", lty = 2, lwd = 2, legend = TRUE)

legend("topright", legend = c("Media"), col = c("red"), lty = c(2), lwd = c(2))


# La densidad de los salarios por kernel

# a. Biweigth

density_biweight <- density(ult.salario,kernel = "biweight")

plot(density_biweight, main = "Biweight kernel", ylab = "Densidad")

hist(ult.salario,
     main = "Histograma de últimos salarios con kernel Biweight",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

lines(density_biweight,lwd=2,lty=1)

# b. Normal o Gaussiano

density_normal <- density(ult.salario,kernel = "gaussian")

plot(density_normal, main = "Normal kernel", ylab = "Densidad")

hist(ult.salario,
     main = "Histograma de últimos salarios con kernel Normal",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

lines(density_normal,lwd=2,lty=1)

# c. Epanechnikov

density_epanechnikov <- density(ult.salario,kernel = "epanechnikov")

plot(density_epanechnikov, main = "Epanechnikov kernel", ylab = "Densidad")

hist(ult.salario,
     main = "Histograma de últimos salarios con kernel Epanechnikov",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

lines(density_epanechnikov,lwd=2,lty=1)

# d. Coseno

density_coseno <- density(ult.salario,kernel = "cosine")

plot(density_coseno, main = "Coseno kernel", ylab = "Densidad")

hist(ult.salario,
     main = "Histograma de últimos salarios con kernel Coseno",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

lines(density_coseno,lwd=2,lty=1)

# e. Rectangular o uniforme

density_uniforme <- density(ult.salario,kernel = "rectangular")

plot(density_uniforme, main = "Kernel Uniforme", ylab = "Densidad")

hist(ult.salario,
     main = "Histograma de últimos salarios con kernel Uniforme",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

lines(density_uniforme,lwd=2,lty=1)

# f. Triangular

density_triangular <- density(ult.salario,kernel = "triangular")

plot(density_triangular, main = "Kernel Triangular", ylab = "Densidad")

hist(ult.salario,
     main = "Histograma de últimos salarios con kernel Triangular",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

lines(density_triangular,lwd=2,lty=1)


# Ahora queremos ver resultados de las densidades en el histograma

hist(ult.salario,
     main = "Histograma de últimos salarios con todos los Kernel",
     xlab = "Salarios",
     ylab = "Densidad",
     col = "lightskyblue2", breaks=20, freq = FALSE, ylim = c(0, 0.0000008))

# f. Triangular
lines(density_triangular, lwd = 2, lty = 1, col = "blue2")

# b. Normal o Gaussiano
lines(density_normal, lwd = 2, lty = 1, col = "red2")

# a. Biweight
lines(density_biweight, lwd = 2, lty = 1, col = "cyan4")

# c. Epanechnikov
lines(density_epanechnikov, lwd = 2, lty = 1, col = "seagreen")

# d. Coseno
lines(density_coseno, lwd = 2, lty = 1, col = "hotpink1")

# e. Rectangular o uniforme
lines(density_uniforme, lwd = 2, lty = 1, col = "goldenrod2")

legend("topright", legend = c("Biweight", "Normal", "Epanechnikov", "Coseno", "Rectangular", "Triangular"),
       col = c("cyan4", "red2", "seagreen", "hotpink1", "goldenrod2", "blue2"),
       lty = 1, lwd = 2)

#----Parte III---------------------------------------------------

# Queremos realizar un análisis AIC de la variable U. Salarios para determinar que densidad paramétrica es la que más se le aproxima.

model_select(
  ult.salario,
  models = univariateML_models,
  criterion = c("aic", "bic", "loglik"),
  na.rm = FALSE,
)

rriskFitdist.cont(ult.salario,"t" ,method = c("mle", "mme"))

