#---Paquetes--------------------------------------------------------

library(stringr) # para cambiar todas las commas a puntos en la columna Ultimo Salario
library(ggplot2) # para hacer graficos
library(cowplot) # facilita creación de gráficos

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



