library(dplyr)
library(ggplot2)

# Para la funcion sample.split
library(caTools)
# Para la función svm
library(e1071)

# Para la funcion rpart y rpart.plot
library(rpart)
library(rpart.plot)

# Para la funcion confusionMatrix
library(caret)

# Para la función roc
library(pROC)

# Para la funcion randomForest
library(randomForest)

# para la fucnion kknn
library(kknn)

#-------------------------------------------------------------------------------
# 1. Se carga el archivo de datos**
#-------------------------------------------------------------------------------

accidentes <- read.csv("temp_5571830814335439232.csv", encoding = "UTF-8", 
                       header = TRUE)


#Se asignan los nombres de las variables**
nombres_columna <- c("a_persona", "rol", "tipo_de_lesion", "edad", 
                     "edad_quinquenal", "sexo", "anno", "mes", "dia", "provincia",
                     "canton", "distrito", "dia_1", "mes_1", "edad_quinquenal_1")
colnames(accidentes) <- nombres_columna
colnames(accidentes)


#Se eliminan las variables que no se requieren**
# Eliminar las variables que no se necesitan
accidentes <- accidentes %>% 
  select(-a_persona, -dia_1, -mes_1, -edad_quinquenal_1)


#Se asignan las etiquetas a las variables de tipo factor**
accidentes$mes <- 
  factor(accidentes$mes, levels = c("Enero","Febrero","Marzo","Abril","Mayo",
                                    "Junio","Julio","Agosto","Setiembre",
                                    "Octubre","Noviembre", "Diciembre"),
         labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
                    "Agosto","Septiembre","Octubre","Noviembre", "Diciembre"))

levels(accidentes$mes)

accidentes$provincia <- 
  factor(accidentes$provincia, levels = c("San José","Alajuela","Cartago",
                                          "Heredia","Guanacaste","Puntarenas",
                                          "Limón"))

#Se identifica la variable objetivo**
# Se revisa la variable tipo_de_lesion para identificar las clases que la componen 
table(accidentes$tipo_de_lesion)

#Se crea una variable binaria que identifica la clase de accidente (si hubieron víctimas).
accidentes <- accidentes %>% 
  mutate(clase_accidente = 
           ifelse (tipo_de_lesion == "Ileso", "Ileso","Víctima"))

# Se convierte a factor la nueva variable
accidentes$clase_accidente <- 
  factor(accidentes$clase_accidente, levels = c("Víctima","Ileso"))

# Se valida la cantidad de observaciones
table(accidentes$clase_accidente)


#Imputacion de datos
# Se imputa el valor desconocido de la variable edad por el promedio
accidentes$edad.mean <- accidentes$edad
accidentes$edad.mean[accidentes$edad.mean == "Desconocido"] <- NA
accidentes$edad.mean[is.na(accidentes$edad.mean)] <- 
  round(mean(as.numeric(accidentes$edad.mean), na.rm = TRUE),digits = 0)
accidentes$edad.mean <- as.integer(as.character(accidentes$edad.mean))


# Rangos de edad de la OMS
# in utero y nacimiento, primera infancia (0-5 años)
# infancia (6 - 11 años), 
# adolescencia (12-18 años), 
# juventud (18 - 26 años), 
# adultez (27 - 59 años) 
# vejez (60 años y más)
accidentes$edad.oms <- as.character(accidentes$edad.mean)
accidentes$edad.oms[accidentes$edad.mean <= 5] <- "Primera Infancia"
accidentes$edad.oms[accidentes$edad.mean > 5 & accidentes$edad.mean <= 11] <- "Infancia"
accidentes$edad.oms[accidentes$edad.mean > 11 & accidentes$edad.mean <= 18] <- "Adolescencia"
accidentes$edad.oms[accidentes$edad.mean > 18 & accidentes$edad.mean <= 26] <- "Juventud"
accidentes$edad.oms[accidentes$edad.mean > 26 & accidentes$edad.mean <= 59] <- "Adultez"
accidentes$edad.oms[accidentes$edad.mean >= 60] <- "Vejez"

accidentes$edad.oms <- 
  factor(accidentes$edad.oms, levels = c("Primera Infancia","Infancia",
                                         "Adolescencia","Juventud","Adultez",
                                         "Vejez"))

#-------------------------------------------------------------------------------
#2. Desarolle el Entendimiento de los Datos
#-------------------------------------------------------------------------------
#**Se identifican las variables y sus tipos de datos**
glimpse(accidentes)

#**Se realiza un resumen de cada variable**
summary(accidentes)


#**Se identifica la cantidad de clases de cada una de las variables**
# Calcula el número de clases para cada variable
as_tibble(arrange(cbind.data.frame(Variables=names(accidentes), 
                                   Numero_Clases=sapply(accidentes,
                                                        function(x){as.numeric(length(levels(x)))})),-Numero_Clases))

#-------------------------------------------------------------------------------
# 3. Utilizando barplot cree un gr?fico de los atributos del dataset, observe las 
# correlaciones entre atributos
#-------------------------------------------------------------------------------

#**Se analizan las variables más representativas**
#barplot(table(accidentes$clase_accidente), main = "Distribución de Accidentes", 
#        ylab = "Observaciones", xlab = "Hay Víctimas?")

# Accidentes por rol
ggplot(as.data.frame(table(accidentes$clase_accidente, accidentes$edad.oms)),
       aes(x = Var2 , y = Freq, fill = Var1 )) +
  geom_bar(stat = "identity")  +
  #coord_flip() + 
  xlab("Rol") + ylab("Cantidad") + 
  ggtitle("Distribución de Clases de Accidente por Edad")  + theme_light() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)#,
        #axis.text.x = element_text(angle=45, vjust=1, hjust=1)
        ) +
  guides(fill=guide_legend(title="Clase Accidente"))

# Anàlisis de la variable provincia
ggplot(accidentes,
       aes(x = edad.oms, fill= clase_accidente  ))+
  geom_bar(position = position_dodge2())+  
  xlab("Provincia") + ylab("Cantidad") + 
  ggtitle("Distribución de Accidentes por Provincia")  + theme_light() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  guides(fill=guide_legend(title="Clase Accidente"))

#-------------------------------------------------------------------------------
#4. Realice al menos 5 modelos de los observados en clase
#-------------------------------------------------------------------------------
#**División de los datos de prueba y entrenamiento**
set.seed(201909)

split <- sample.split(accidentes$clase_accidente, SplitRatio = 0.8 )
entrena <- subset(accidentes, split == TRUE)
prueba <- subset(accidentes, split == FALSE)

#**Cantidad de observaciones en el conjunto de prueba (FALSE) y el de entrenamiento (TRUE)**
table(accidentes$clase_accidente, split)


#**Creación de modelo utilizando método Random Forest**
#-------------------------------------------------------------------------------
#Random Forest
modelo_bosque <- randomForest(clase_accidente ~ rol + provincia + anno + sexo + 
                                edad_quinquenal, data = entrena)
modelo_bosque
plot(modelo_bosque)


#**Creación de modelo utilizando método Arbol de decision**
#-------------------------------------------------------------------------------
# Arbol de decisiones 
modelo_arbol <- rpart(clase_accidente ~ .,#edad_quinquenal + rol + mes + provincia + sexo, 
                      data = entrena, method =  'class')
modelo_arbol

rpart.plot(modelo_arbol, shadow.col = "gray", extra = 104, 
           main = "Clasificación de Accidentes por Clase de Accidente")


#**Creación de modelo utilizando método SVM**
#-------------------------------------------------------------------------------

# Reduccion de datos para SVM
split <- sample.split(accidentes$clase_accidente, SplitRatio = 0.05 )
accidentes_svm <- subset(accidentes, split == TRUE)

split <- sample.split(accidentes_svm$clase_accidente, SplitRatio = 0.8 )
entrena_svm <- subset(accidentes_svm, split == TRUE)
prueba_svm <- subset(accidentes_svm, split == FALSE)

# SVM
data_svm <- entrena_svm[,names(entrena_svm) %in% c("clase_accidente","anno","mes","edad.mean")]

modelo_svm <- svm(clase_accidente ~ ., data = data_svm,
                  kernel = "linear",  cost = 1, gamma = 0.5)
modelo_svm

plot(modelo_svm, data_svm, anno ~ edad.mean )





#**Creación del modelo utilizando método de KNN (vecinos más cercanos )**
#-------------------------------------------------------------------------------
modelo_knn <- kknn(clase_accidente ~ rol + provincia , train = entrena, 
                   test = prueba, k = 4)


#**Creación del modelo de red neuronal**
#-------------------------------------------------------------------------------
modelo_neuronal <- 
  nnet::nnet(clase_accidente ~ rol + sexo + provincia , data = entrena,
             size = 2, maxit = 10000, decay = 0.001, rang = 0.05, 
             na.action = na.omit, skip = TRUE)


#-------------------------------------------------------------------------------
#5. Evaluaci?n de los modelos
#-------------------------------------------------------------------------------

#**Evaluación del modelo Random Forest** 
#-------------------------------------------------------------------------------
#Random Forest
predic_bosque <- predict(modelo_bosque, newdata = prueba, type = 'class')

confusionMatrix(data = predic_bosque, reference = prueba$clase_accidente,
                positive = "Víctima" )


## Prediccion ROC
predic_bosqueROC = prediction(c(predic_bosque), c(prueba[,"clase_accidente"]))
as.numeric(performance(predic_bosqueROC, "auc")@y.values)

plot(performance(predic_bosqueROC, "tpr", "fpr"),
     colorize = T,
     print.cutoffs.at = seq(0,1,by = 0.1),
     text.adj = c(-0.2,1.7),
     main = 'Curva ROC del modelo con el método Árboles Aleatorios')


## Prediccion ROC con pROC
fitted = attributes(predict(modelo_bosque, prueba))

glimpse(as.integer(fitted$names))

pROC_obj <- roc( prueba[,"clase_accidente"],#df$labels,
                 as.integer(fitted$names),##df$predictions,
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci, type="bars")

## Prediccion ROCR
pred <- prediction(as.integer(fitted$names), prueba[,"clase_accidente"])
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

# Prediction 
library(precrec)
precrec_obj2 <- evalmod(scores = as.integer(fitted$names), 
                        labels = prueba[,"clase_accidente"], mode="basic")
autoplot(precrec_obj2)


library(plotROC)
rocplot <- ggplot(prueba, aes(m = as.integer(fitted$names), d = prueba[,"clase_accidente"]))+ 
  geom_roc(n.cuts=20,labels=FALSE)
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink") 


#**Evaluación del modelo Arbol de decisión**
#-------------------------------------------------------------------------------
predic_arbol <- predict(modelo_arbol, newdata = prueba, type = "class")
confusionMatrix(data = predic_arbol, reference = prueba$clase_accidente,
                positive = "Víctima" )

## Prediccion ROC
prediccionesROC = prediction(c(predic_arbol), c(prueba[,"clase_accidente"]))
as.numeric(performance(prediccionesROC, "auc")@y.values)

plot(performance(prediccionesROC, "tpr", "fpr"),
     colorize = T,
     print.cutoffs.at = seq(0,1,by = 0.1),
     text.adj = c(-0.2,1.7),
     main = 'Curva ROC del modelo')

# Tune de arbol de decisión
tune_arbol_1 <- tune.rpart(clase_accidente ~ edad_quinquenal + rol + mes + 
                             provincia + sexo, 
                           data = prueba, minsplit = c(1,5,10,15))

tune_arbol_2 <- tune.rpart(clase_accidente ~ .,
                           data = prueba, minsplit = c(1,5,10,15))
summary(tune_arbol_1)
summary(tune_arbol_2)

# Gráfico de comparación de tunning con variables
par(mfrow = c(1, 2))
plot(tune_arbol_1)
plot(tune_arbol_2)


# Otros gráficos
glimpse(predic_arbol)
summary(predic_arbol)
class(predic_arbol[1])
glimpse(as.data.frame(predic_arbol))

## Prediccion ROC ggplot
library(plotROC)
rocplot <- ggplot(prueba, aes(m = predic_arbol, d = prueba[,"clase_accidente"])) + 
  geom_roc(n.cuts=20,labels=FALSE)
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink") 



#Evaluación de modelo SVM
#-------------------------------------------------------------------------------
data_prueba_svm <- prueba_svm[,names(prueba_svm) %in% c("clase_accidente","anno","mes","edad.mean")]

predic_svm <- predict(modelo_svm, newdata = prueba_svm, type = 'class')
table(predic_svm, prueba_svm$clase_accidente)


modelo_svm_op <- svm(clase_accidente ~ ., data = data_svm, kernel = "linear",  
                     cost = 1, gamma = 1, decision.values = TRUE)

fitted = attributes(predict(modelo_svm_op, data_prueba_svm, decision.values = TRUE))$decision.values


#-------------------------------------------------------------------------------
pROC_obj <- roc( data_prueba_svm[,names(data_prueba_svm) %in% c("clase_accidente")],#df$labels,
                 fitted,##df$predictions,
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci, type="bars")

# Predic
predic_svmROC <- predict(modelo_svm, newdata = prueba_svm, type = "class")
confusionMatrix(data = predic_svmROC, reference = prueba_svm$clase_accidente,
                positive = "Víctima" )

## Prediccion ROC
prediccionesROC = prediction(c(predic_svmROC), c(prueba_svm[,"clase_accidente"]))
as.numeric(performance(prediccionesROC, "auc")@y.values)

plot(performance(prediccionesROC, "tpr", "fpr"),
     colorize = T,
     print.cutoffs.at = seq(0,1,by = 0.1),
     text.adj = c(-0.2,1.7),
     main = 'Curva ROC del modelo')



# Tune SVM
tune.out <- tune(svm, clase_accidente ~ ., data = data_svm, kernel = "linear",
                 ranges = list(cost = c(0.1, 1, 5), gamma = c(0.5, 1)))

summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)




#**Evaluación del modelo KNN (vecinos más cercanos )**
#-------------------------------------------------------------------------------
fit <- fitted(modelo_knn)
confusionMatrix(prueba$clase_accidente, fit, positive = "Víctima")

predic_knn <- predict(modelo_knn, newdata = prueba)
confusionMatrix(data = predic_knn, reference = prueba$clase_accidente,
                positive = "Víctima" )


## Prediccion ROC
predic_knnROC = prediction(c(predic_knn), c(prueba[,"clase_accidente"]))

plot(performance(predic_knnROC, "tpr", "fpr"),
     colorize = T,
     print.cutoffs.at = seq(0,1,by = 0.1),
     text.adj = c(-0.2,1.7),
     main = "Curva ROC del modelo con el método Vecinos más Cercanos(KNN)")


# Tune KNN
#tune_knn <- tune.knn(entrena[,c("rol","provincia")], 
#                      entrena[,"clase_accidente"], 
#                      k = 1:5, tunecontrol = tune.control(sampling = "boot"))
#summary(tune_knn)
#plot(tune_knn)


#**Evaluación del modelo de la red neuronal**
#-------------------------------------------------------------------------------
pedict_neuronal <- predict(modelo_neuronal, newdata = prueba ,type = "class")
table(prueba$clase_accidente, pedict_neuronal, dnn = c("Actual","Predicho"))

confusionMatrix( data = factor(pedict_neuronal), reference = prueba$clase_accidente, 
                 positive = "Víctima")

## Prediccion ROC
predic_neuronalROC = prediction(c(predict(modelo_neuronal, newdata = prueba )), 
                                c(prueba[,"clase_accidente"]))

plot(performance(predic_neuronalROC, "tpr", "fpr"),
     colorize = T,
     print.cutoffs.at = seq(0,1,by = 0.1),
     text.adj = c(-0.2,1.7),
     main = "Curva ROC del modelo con el método Red Neuronal")

# Tune Red Neuronal
#tune_nnet <- tune.nnet(entrena[,c("rol","provincia")],
#                       entrena[,"clase_accidente"], 
#                       k = 1:5, tunecontrol = tune.control(sampling = "boot"))


