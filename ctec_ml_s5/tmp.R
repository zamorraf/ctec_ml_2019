library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(neuralnet)
library(ROCR)
library(caret)

# 1. Carga del archivo
# ------------------------------------------------------------------------------

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
agaricus_lepiota <- read.table(url, header = FALSE,  sep = ",", stringsAsFactors = FALSE)
agaricus_lepiota <- read.csv("agaricus-lepiota.data", header = FALSE)
#write.table(mushroomsDB, file = "Mushroom.csv", sep = ",")


# Carga de los nombres de columnas
nombres_columna <- c("comestibilidad","cap_shape","cap_surface","cap_color","bruises",
                     "odor","gill_attachment","gill_spacing","gill_size", 
                     "gill_color","stalk_shape","stalk_root",
                     "stalk_surface_above_ring","stalk_surface_below_ring",
                     "stalk_color_above_ring","stalk_color_below_ring",
                     "veil_type","veil_color","ring_number","ring_type",
                     "spore_print_color","population","habitat")

names(agaricus_lepiota) <- nombres_columna

# Etiquetas de los factores
agaricus_lepiota$comestibilidad <- factor(agaricus_lepiota$comestibilidad,
                                 levels = c("p","e"),
                                 labels = c("Venenoso","Comestible"))

agaricus_lepiota$cap_shape <- factor(agaricus_lepiota$cap_shape,
                                     levels = c("b","c","f","k","s","x"),
                                     labels = c("bell", "conical", "flat", 
                                                "knobbed", "sunken", "convex"))

agaricus_lepiota$odor <- factor(agaricus_lepiota$odor,
                                levels = c("a","c","f","l","m","n","p","s","y"),
                                labels = c("almond", "creosote", "foul", "anise", 
                                           "musty", "none", "pungent", "spicy", 
                                           "fishy"))

agaricus_lepiota$spore_print_color <- factor(agaricus_lepiota$spore_print_color,
                                            levels = c("k","n","b","h","r","o",
                                                       "u","w","y"),
                                            labels = c("black","brown","buff",
                                                       "chocolate","green",
                                                       "orange","purple","white",
                                                       "yellow"))



# 2. Entendimiento de los datos
# ------------------------------------------------------------------------------
# Estadisticas de las datos
glimpse(agaricus_lepiota)


table(agricus_lepiota$gill_attachment)

summary(agaricus_lepiota[1:5])
summary(agaricus_lepiota[6:10])
summary(agaricus_lepiota[10:15])
summary(agaricus_lepiota[16:23])


# 3. Utilizando barplot cree un gráfico de los atributos del dataset, observe las 
# correlaciones entre atributos
#-------------------------------------------------------------------------------
barplot(table(agaricus_lepiota$comestibilidad),
        main = 'Distribución de Comestibilidad de los Hongos',
        ylab = 'Observaciones',
        xlab = '¿Comestibilidad?')

table(agaricus_lepiota$comestibilidad ,agaricus_lepiota$cap_shape)

barplot(table(agaricus_lepiota$comestibilidad ,agaricus_lepiota$cap_shape), 
        col = c("green","red"), 
        main = "Distribucion de cap_shape",
        xlab = "cap_shape", ylab = "Cantidad",
        beside = TRUE)
legend(x = "topleft",c("Comestible","Venenoso"), fill = c("green","red"))


ggplot(agaricus_lepiota, aes(x = comestibilidad, y = cap_shape, col = comestibilidad)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))


# 4. Realice al menos 3 modelos vistos en clase
set.seed(201908)
split <- sample.split(agaricus_lepiota$comestibilidad, SplitRatio = 0.8)
entrena <- subset(agaricus_lepiota, split == TRUE)
prueba <- subset(agaricus_lepiota, split == FALSE)


#Random Forest
modelo_bosque <- randomForest(comestibilidad ~ ., data = entrena )
modelo_bosque
plot(modelo_bosque)

# Modelo utilizando Arboles de decision
modelo_arbol <- rpart(comestibilidad ~ .,data = entrena[,-which(colnames(entrena) == "cap_shape")] , 
                      method =  'class')


modelo_arbol <- rpart(comestibilidad ~ .,data = entrena, method =  'class')
modelo_arbol

rpart.plot(modelo_arbol, shadow.col = "gray", extra = 104,  box.palette = "GnBu", 
           branch.lty = 3,  nn = TRUE,
           main = "Clasificación de Hongos")



# Modelo SVM
library(e1071)
model_svm <- svm(comestibilidad ~ cap_shape + bruises + gill_attachment, 
                 data = agaricus_lepiota, 
                 cost = 1000, gamma = 0.01)


# 5. Evaluacion
#Random Forest
predic_bosque <- predict(modelo_bosque, newdata = prueba, type = 'class')

table(predic_bosque, prueba$comestibilidad)

caret::confusionMatrix(data = predic_bosque, reference = prueba$comestibilidad,
                       positive = "Comestible" )

# Arbol de decisiones
predic_arbol <- predict(modelo_arbol, newdata = prueba, type = 'class')
table(predic_arbol ,prueba$comestibilidad )

caret::confusionMatrix(data = predic_arbol, reference = prueba$comestibilidad,
                       positive = "Comestible" )

