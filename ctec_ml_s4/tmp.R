library('ggplot2')
library(dplyr)
library(visdat)
library(GGally)
library(reshape2)
library(ggplot2)
library(readr)

datos <- read.csv("Wholesale.csv", header = TRUE)
datos



glimpse(datos)
head(datos)
summary(datos)



datos$Channel <- factor(datos$Channel, 
                        levels = c(1,2),
                        labels = c("HoReCa","Retail")) 

datos$Region <- factor(datos$Region, 
                       levels = c(1,2,3),
                       labels = c("Lisbon","Oporto","Other Region")) 

vis_dat(datos)

table(datos$Channel)
table(datos$Region)




# Correlación
cor(datos)

ggpairs(datos, binwidth = 50)


barplot(melt(datos))
barplot(scale(datos[,c(3:8)]))


#Grafico
bxplt <- ggplot(data = melt(datos), aes(x= variable, y = value))
bxplt <- bxplt + geom_boxplot()
bxplt <- bxplt + facet_wrap(~Channel)
bxplt <- bxplt + coord_flip()
bxplt <- bxplt + labs(x = 'producto', y  = 'ventas')
bxplt <- bxplt + ggtitle('Ventas por producto y por canal')
bxplt


# Respuesta 6

#5 Centroides utilizando k-means
set.seed(2019)
modelo.agrupacion <- kmeans(datos[,c(3:8)], centers = 5, nstart = 20)

#predecir utilizando el modelo para agregar una variable nueva llamada 'cluster' al conjunto de datos
#Wholesale$cluster <- modelo.agrupacion$cluster
datos <- datos %>% 
  mutate(cluster = modelo.agrupacion$cluster)

#convertir la variable nueva a un factor
datos$cluster <- factor(datos$cluster)

devtools::install_github("kassambara/factoextra")

library(factoextra)
fviz_cluster(modelo.agrupacion, data = datos[,c(3:8)])

plot(datos[,c(3:8)],
     col = datos$cluster)

#Respuesta 6 .2
modelo.agrupacion <- kmeans(datos[,c(3:8)], centers = 5)

# Respuesta 7
as_tibble( aggregate(datos[,c(3:8)], by = list(cluster = modelo.agrupacion$cluster ), summary))


#Respuesta 8
table(datos$cluster,datos$Milk)

datos %>% 
  select(Milk,cluster) %>% 
  group_by(cluster) %>% 
  summarise(Suma = sum(Milk)) %>% 
  arrange(desc(Suma))
  

* Ejemplo:
  - Las principales características de cada grupo son:
  - #### Grupo #1: (2do grupo más pequeño)
  - Principalmente observaciones de la región 3 y el canal 1.
- Muy alto consumo de productos frescos.
- Bajo consumo de leche.
- Bajo consumo de comestibles.
- Alto consumo de alimentos congelados.
- Bajo consumo de detergentes.
- Bajo consumo de delicassen.


