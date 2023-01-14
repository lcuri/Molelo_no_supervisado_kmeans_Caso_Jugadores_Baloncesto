##                           MODELO CLUSTERIZACION-ALGORITMO KMEANS-CASO BALONCESTO
#                                                                                              by Luis Curi
## 1. PREPARACION DEL ENTORNO
##############################################################

##limpiar mi area de trabajo
rm(list = ls())

## LLAMAR PAQUETES NECESARIOS
library(ggplot2)

## establecer directorio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## llamar a la base
base<-read.csv("basketball.csv")

#############################################################
## ESTADISTICA DESCRIPTIVA BASICA
#############################################################
##conocer los datos
str(base) #todos son numericos
summary(base) #vieño maximos, minimos y concentraciones
xray::distributions(base[,1:5]) #representando su distribucion grafica
boxplot(base, las = 2) #buscando outliers y diferencia de dimensiones

##Normalizacion de datos
   ##crear la funcion de normalizacion
normalizar = function(x) {
  return ((x-min(x))/ (max(x)- min(x)))
}
  ##normalizar y crear colocarlo e otra base
base1<-as.data.frame(lapply(base[,1:5], normalizar))  ##lappy aplica una funcion a todos los elementos de una lista
##en este caso aplica la normalizacion a todas las columnas seleccionados de la base (del 1 al 5)

  ##Vizualizar las base normalizada
boxplot(base1)
xray::distributions(base[,1:5]) ##el paquete xray permite analizar las variables de un conjunto de datos

############################################################
## APLICACION DE CLUSTERIZACION - KMEANS (basado en centroides)
##############################################################

## 1. Calcular el numero de agrupaciones optimo

##FORMA 1:
    #crear 2 vectores
inter = c();
intra = c();

   #aqui lo que hacemos es aplicar kmeans con perfiles del 1 al 10, almacenarnos en vectores y ver cual es el mas optimo
for (i in 1:10)
{
  out = kmeans(base, i)
   ##append agrega los diferentes tipos de valores enteros en un vector
  intra = append(intra, out$tot.withinss); # tot.withinss nos proporciona la suma de cuadrados totales
  inter = append(inter, out$betweenss);  #betweenss nos proporciona la sumas de cuadrados dentro de cada grupos
}

plot(intra, type = "b", col = "red")
lines(inter, type = "b", col = "green")

## forma 2 : Indice de silueta (evaluacion= indice silueta individual, indice de silueta general, cantidad balanceada de elementos entre
     #clusters y la no existencia de observaciones negativas)
library(cluster)
      ##calculo de distancias euclideanas
diss.base1 = daisy(base1) ##daysi toma las distancias para ambos tipos de datos
diss.base1
      ##Graficar las siluetas para grupos de 2 a 10
par(mfrow = c(1:2))
for (i in 2:10)
{
  modelo = kmeans(base1, centers = i)
  plot(silhouette(modelo$cluster, diss.base1))
}
       
#Forma 3: (de la libreria fpc)
       ##criterio automatico para estimar el k optimo.
kmeansruns(base1, criterion = "asw", nstart = 100) #con 100 inicializaciones (las unicializaciones dependen del tamaño de filas)

#criterio 4: criterio de kalinski harabasz
kmeansruns(base1, criterion = "ch", nstart = 100)  #kalinsky determinó un optimo de 2 clusters

  ##el numero optimo de perfiles será el que cruce la suma de cuadrados totales con la suma de cuadradospor grupo 
  ## podemos concluir que es optimo realizar 4 pefiles de jugadores


## 2. Aplicamos el algoritmo
set.seed(123) ##plantamos una semilla para que volvamos a tener el mismo resultado
out = kmeans(base1,4) #aplicamos kmeans en la base normalizada para que cree 4 perfiles
out
      ##matriz de dispersion de cruce de variables
pairs(base1, col=out$cluster) #comparando clusterizaciones por variable, para visualizar a que variable se esta asignando mejor los grupos
pairs(base[,1:5], col=base$cluster) #seria lo mismo que el grafico anterior, pero con datos no normalizados (col hace refencia a color)

      #Aqui podemos observar los centroides de los grupos
out$centers ##centroides es el valor medio del grupo asignado a cada oservacion


## 3. Vizualizar los grupos creados

      ##visualizar los graficos
library(clusterSim)
clusplot(base1, out$cluster, color=TRUE,  shade = TRUE, labels = 2,  lines = 0)



## 4 Perfilamiento y caracterizacion de clusters
  
      ##colocar en el dataframe los resultados para tomar desiciones baso en la creacion de perfiles
base$cluster <-out$cluster

      ##con la funcion aggregate hacemos un group by en base al numero de cluster asignado y a cada grupo encontrado le sacamos el promedio
aggregate(base, by=list(base$cluster), FUN = mean)

#Group.1 assists_per_minute   height time_played      age points_per_minute cluster
#1       1          0.2421150 183.1000    33.66400 27.90000         0.4188800       1
#2       2          0.1271346 194.6538    33.68962 28.00000         0.5272923       2
#3       3          0.1416765 191.4706    18.98059 32.29412         0.3418059       3
#4       4          0.1493152 189.3939    18.75091 25.09091         0.3772061       4

#el grupo 1 tiene a individuos con mayor asistencia por minuto 
#el grupo 2 tiene a individuos mas altos, seguido del grupo 3
#el grupo 1 y 2 tiene a indiviuos con mejores tiempos
#el grupo 4 tiene a los jugadores mas jovenes
#el grupo 2 tiene a los individuos con mayor anotaciones por minuto.

  ##boxplot para expresar diferencias entre grupos

par(mfrow= c(1:2))
for (i in 1:5)
{
  boxplot(base[,i] ~ base$cluster,
  main = names(base[i]), xlab = "Perfiles", ylab = "")
}

##################################################################################################################


