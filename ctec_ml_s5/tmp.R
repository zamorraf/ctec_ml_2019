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
agaricus_lepiota$comestibilidad <- 
  factor(agaricus_lepiota$comestibilidad,
         levels = c("p","e"),
         labels = c("Venenoso","Comestible"))

agaricus_lepiota$cap_shape <- 
  factor(agaricus_lepiota$cap_shape,
         levels = c("b","c","f","k","s","x"),
         labels = c("bell","conical","flat","knobbed","sunken","convex"))

agaricus_lepiota$cap_surface <- 
  factor(agaricus_lepiota$cap_surface,
         levels = c("f","g","y","s"),
         labels = c("fibrous", "grooves", "scaly","smooth"))

agaricus_lepiota$cap_color <- 
  factor(agaricus_lepiota$cap_color,
         levels = c("n","b","c","g","r","p","u","e","w","y"),
         labels = c("brown", "buff", "cinnamon","gray","green","pink",
                    "purple","red","white", "yellow"))

agaricus_lepiota$bruises <- 
  factor(agaricus_lepiota$bruises,
         levels = c("t","f"),
         labels = c("bruises", "no"))

agaricus_lepiota$odor <- 
  factor(agaricus_lepiota$odor,
         levels = c("a","c","f","l","m","n","p","s","y"),
         labels = c("almond", "creosote", "foul", "anise","musty", "none",
                    "pungent","spicy","fishy"))

agaricus_lepiota$gill_attachment <- 
  factor(agaricus_lepiota$gill_attachment,
         levels = c("a","d","f","n"),
         labels = c("attached", "descending","free","notched"))

agaricus_lepiota$gill_spacing <- 
  factor(agaricus_lepiota$gill_spacing,
         levels = c("c","w","d"),
         labels = c("close", "crowded","distant"))

agaricus_lepiota$gill_size <- 
  factor(agaricus_lepiota$gill_size,
         levels = c("b","n"),
         labels = c("broad", "narrow"))

agaricus_lepiota$gill_color <- 
  factor(agaricus_lepiota$gill_color,
         levels = c("k","n","b","h","g","r","o","p","u","e","w","y"),
         labels = c("black", "brown","buff","chocolate","gray","green",
                    "orange","pink","purple","red","white","yellow"))

agaricus_lepiota$stalk_shape <- 
  factor(agaricus_lepiota$stalk_shape,
         levels = c( "e","t"),
         labels = c("enlarging","tapering"))

agaricus_lepiota$stalk_root <- 
  factor(agaricus_lepiota$stalk_root,
         levels = c("b","c","u","e","z","r","?"),
         labels = c("bulbous","club","cup","equal","rhizomorphs","rooted",
                    "missing"))

agaricus_lepiota$stalk_surface_above_ring <- 
  factor(agaricus_lepiota$stalk_surface_above_ring,
         levels = c("f","y","k","s"),
         labels = c("fibrous","scaly","silky","smooth"))

agaricus_lepiota$stalk_surface_below_ring <- 
  factor(agaricus_lepiota$stalk_surface_below_ring,
         levels = c("f","y","k","s"),
         labels = c("fibrous","scaly","silky","smooth"))

agaricus_lepiota$stalk_color_above_ring <- 
  factor(agaricus_lepiota$stalk_color_above_ring,
         levels = c("n","b","c","g","o","p","e","w","y"),
         labels = c("brown","buff","cinnamon","gray","orange","pink","red",
                    "white","yellow"))

agaricus_lepiota$stalk_color_below_ring <- 
  factor(agaricus_lepiota$stalk_color_below_ring,
         levels = c("n","b","c","g","o","p","e","w","y"),
         labels = c("brown","buff","cinnamon","gray","orange","pink","red",
                    "white","yellow"))

agaricus_lepiota$veil_type <- 
  factor(agaricus_lepiota$veil_type,
         levels = c("p","u"),
         labels = c("partial","universal"))

agaricus_lepiota$veil_color <- 
  factor(agaricus_lepiota$veil_color,
         levels = c("n","o","w","y"),
         labels = c("brown","orange","white","yellow"))

agaricus_lepiota$ring_number <- 
  factor(agaricus_lepiota$ring_number,
         levels = c("n","o","t"),
         labels = c("none","one","two"))

agaricus_lepiota$ring_type <- 
  factor(agaricus_lepiota$ring_type,
         levels = c("c","e","f","l","n","p","s","z"),
         labels = c("cobwebby","evanescent","flaring","large","none",
                    "pendant","sheathing","zone"))

agaricus_lepiota$spore_print_color <- 
  factor(agaricus_lepiota$spore_print_color,
         levels = c("k","n","b","h","r","o","u","w","y"),
         labels = c("black","brown","buff","chocolate","green","orange",
                    "purple","white","yellow"))

agaricus_lepiota$population <- 
  factor(agaricus_lepiota$population,
         levels = c("a","c","n","s","v","y"),
         labels = c("abundant","clustered","numerous","scattered",
                    "several","solitary"))

agaricus_lepiota$habitat <- 
  factor(agaricus_lepiota$habitat,
         levels = c("g","l","m","p","u","w","d"),
         labels = c("grasses","leaves","meadows","paths","urban","waste",
                    "woods"))



# 2. Entendimiento de los datos
# ------------------------------------------------------------------------------
# Estadisticas de las datos
glimpse(agaricus_lepiota)


table(agricus_lepiota$gill_attachment)

summary(agaricus_lepiota)


length(levels(agaricus_lepiota$comestibilidad))


# Calcula el número de clases para cada variable
cbind.data.frame(Variables=names(agaricus_lepiota), 
                 Numero_Clases=sapply(agaricus_lepiota,
                                      function(x){as.numeric(length(levels(x)))}))

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

table(agaricus_lepiota$comestibilidad, split)


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

prp(modelo_arbol, type = 2, extra = 104, nn = TRUE, fallen.leaves = TRUE,
    faclen = 4, varlen = 8, shadow.col = "gray")


# Modelo SVM
library(e1071)
modelo_svm <- svm(comestibilidad ~ ., 
                 data = entrena[,-which(colnames(entrena) == "veil_type")], 
                 cost = 1000, gamma = 0.01)
modelo_svm


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

# Modelo SVM
predic_svm <- predict(modelo_svm, newdata = prueba, type = 'class')
table(predic_svm, prueba$comestibilidad)

caret::confusionMatrix(data = predic_svm, reference = prueba$comestibilidad,
                       positive = "Comestible" )
