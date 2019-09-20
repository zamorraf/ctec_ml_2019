library(dplyr)
library(readr)
library(visdat)
library(ggplot2)
# Para la funcion sample.split
library(caTools)
# Para la funcion randomForest
library(randomForest)
# Para la funcion confusion Matrix
library(caret)
# Para la funcion rpart y rpart.plot
library(rpart)
library(rpart.plot)
# Para la función svm
library(e1071)

#-------------------------------------------------------------------------------
# 1.Cargue el archivo nombre.csv en una variable
#-------------------------------------------------------------------------------
charset <- guess_encoding("temp_5571830814335439232.csv" )

accidentes <- read.csv("temp_5571830814335439232.csv", encoding = "UTF-8", 
                       header = TRUE)




nombres_columna <- colnames(accidentes)

nombres_columna <- c("a_persona", "rol", "tipo_de_lesion", "edad", 
                     "edad_quinquenal", "sexo", "anno", "mes", "dia", "provincia",
                     "canton", "distrito", "dia_1", "mes_1", "edad_quinquenal_1")

colnames(accidentes) <- nombres_columna

accidentes$dia == accidentes$día_1

accidentes[,c(9,13)]
accidentes[,c("dia","dia_1")]
accidentes[,"dia"]
substr(accidentes[,"dia_1"],3 ,stop = 20 )
resultado <- accidentes[,"dia"] == substr(accidentes[,"dia_1"],3 ,stop = 20 )[FALSE]

resultado[FALSE]


# diferentes formas de eliminar variables
accidentes <- accidentes[,-1]
accidentes <- accidentes[,!(names(accidentes) %in% "a_persona")]
accidentes <- select(accidentes,-a_persona)


# Eliminar las variables que no se necesitan
accidentes <- accidentes %>% 
                select(-a_persona, -dia_1, mes_1, edad_quinquenal_1)


# Convertir a factor el año (variable anno)
accidentes$anno <- factor(accidentes$anno)


# Asignación de etiquetas y orden
accidentes$anno <- 
  factor(accidentes$anno, levels = c("2013","2014","2015","2016","2017"), 
         labels = c("2013","2014","2015","2016","2017"), ordered = TRUE)

accidentes$mes <- 
  factor(accidentes$mes, levels = c("Enero","Febrero","Marzo","Abril","Mayo",
                                    "Junio","Julio","Agosto","Setiembre",
                                    "Octubre","Noviembre", "Diciembre"),
         labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
                    "Agosto","Septiembre","Octubre","Noviembre", "Diciembre"),
         ordered = TRUE)

accidentes$provincia <- 
  factor(accidentes$provincia, levels = c("San José","Alajuela","Cartago",
                                          "Heredia","Guanacaste","Puntarenas",
                                          "Limón"))


# Se revisa la variable tipo_de_lesion y se convierte a binaria
accidentes$tipo_de_lesion
levels(accidentes$tipo_de_lesion)
table(accidentes$tipo_de_lesion)
accidentes <- accidentes %>% 
                mutate(clase_accidente = 
                         ifelse (tipo_de_lesion == "Ileso", "Ileso","Víctima"))
table(accidentes$clase_accidente)

# Se convierte a factor la nueva variable
accidentes$clase_accidente <- 
  factor(accidentes$clase_accidente, levels = c("Víctima","Ileso"))

# Imputacion de datos
# Se imputa el valor desconocido de la variable edad por el promedio
accidentes$edad.mean <- ifelse(accidentes$edad == "Desconocido",
                               NA,
                               accidentes$edad)

accidentes$edad.mean <- 
  ifelse(is.na(accidentes$edad.mean),
         round(mean(accidentes$edad.mean, na.rm = TRUE),digits = 0),
         accidentes$edad.mean)


# Se escalan las 2 variables numericas
standar.features <- scale(accidentes[,names(accidentes) %in% c("anno","edad.mean")])

accidentes_tmp <- accidentes[names(accidentes) %in% c("clase_accidente")]
accidentes_tmp <- cbind(accidentes_tmp,standar.features)
colnames(accidentes_tmp)[2] <- "anno.scale"
colnames(accidentes_tmp)[3] <- "edad.scale"
accidentes <- cbind(accidentes,accidentes_tmp)

glimpse(accidentes_tmp)
head(standar.features)
glimpse(standar.features)



# 2.Desarolle el Entendimiento de los Datos
glimpse(accidentes)
summary(accidentes)
head(accidentes)

summary(accidentes$edad)
table(accidentes$edad)
dim(accidentes$edad)

# Calcula el número de clases para cada variable
as_tibble(arrange(cbind.data.frame(Variables=names(accidentes), 
                 Numero_Clases=sapply(accidentes,
                                      function(x){as.numeric(length(levels(x)))})),-Numero_Clases))
clases
head(clases)
as_tibble(clases)
table(accidentes$mes)
vis_dat(accidentes, warn_large_data = FALSE)

#-------------------------------------------------------------------------------
# 3. Correlaciòn de los datos
#-------------------------------------------------------------------------------

table(accidentes$edad_quinquenal)


barplot(table(accidentes$clase_accidente), main = "Distribución de Accidentes", 
        ylab = "Observaciones", xlab = "Clase Accidente")

ggplot(as.data.frame(table(accidentes$clase_accidente)), aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity") + 
  xlab("Clase de Accidente") + ylab("Observaciones") + 
  ggtitle("Distribución de Accidentes")  + theme_light() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5))



barplot(table(accidentes$clase_accidente, accidentes$rol), 
        col = c("green","red"), 
        main = "Distribución de Clases de Accidente por Rol", 
        ylab = "Cantidad", xlab = "Rol", las = 2,
        beside = TRUE)
legend(x = "topright",c("Ileso","Víctima"), fill = c("green","red"))


ggplot(as.data.frame(table(accidentes$clase_accidente, accidentes$rol)),
       aes(x = Var2 , y = Freq, fill = Var1 )) +
  geom_bar(stat = "identity")  +

ggplot(accidentes,aes(x = rol, fill= clase_accidente  )) +
  geom_bar(position = position_dodge2())+
  xlab("Provincia") + ylab("Cantidad") + 
  ggtitle("Distribución de Clases de Accidentes por Rol")  + theme_light() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle=45, vjust=1, hjust=1)) +
  guides(fill=guide_legend(title="Clase Accidente"))




barplot(table(accidentes$victimas, accidentes$provincia), 
        col = c("green","red"),
        main = "Distribución de Víctimas por Año", 
        ylab = "Cantidad", xlab = "Año",
        beside = TRUE)

ggplot(as.data.frame(table(accidentes$victimas, accidentes$provincia)),
       aes(x = Var2 , y = Freq, fill = Var1 )) +
  geom_bar(stat = "identity")  +
  #coord_flip() + 
  xlab("Provincia") + ylab("Cantidad") + 
  ggtitle("Distribución de Accidentes por Provincia")  + #theme_light() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  guides(fill=guide_legend(title="Víctima"))
  #scale_fill_brewer(palette = 11, name = "Prueba")


ggplot(as.data.frame(table(accidentes$victimas, accidentes$provincia)),
       aes(x = Var2 , y = Freq ,group = Var1  ))+
  geom_col(position = "dodge", fill = "grey50", colour = "black")
  geom_bar(position = position_dodge2())


ggplot(accidentes,
  aes(x = provincia, fill= clase_accidente  ))+
  geom_bar(position = position_dodge2())+  
  xlab("Provincia") + ylab("Cantidad") + 
  ggtitle("Distribución de Accidentes por Provincia")  + theme_light() + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  guides(fill=guide_legend(title="Clase Accidente"))

#-------------------------------------------------------------------------------
# 4 Modelos
#-------------------------------------------------------------------------------
#**División de los datos de prueba y entrenamiento**
set.seed(201909)
split <- sample.split(accidentes$clase_accidente, SplitRatio = 0.8 )
entrena <- subset(accidentes, split == TRUE)
prueba <- subset(accidentes, split == FALSE)

#**Cantidad de observaciones en el conjunto de prueba (FALSE) y el de entrenamiento (TRUE)**
table(accidentes$clase_accidente, split)


#**Creación de modelo utilizando método Random Forest**
#Random Forest
modelo_bosque <- randomForest(clase_accidente ~ rol + provincia + sexo + 
                              edad_quinquenal, data = entrena)
modelo_bosque
plot(modelo_bosque)


#**Creación de modelo utilizando método Arboles de decisión**
# Arbol de decisiones 
modelo_arbol <- rpart(clase_accidente ~ edad_quinquenal + rol + mes + provincia + 
                      sexo, data = entrena, method =  'class')
modelo_arbol
rpart.plot(modelo_arbol, shadow.col = "gray", extra = 104, 
           main = "Clasificación de Accidentes por Clase de Accidente")



#**Creación de modelo utilizando método SVM**
modelo_svm <- svm(clase_accidente ~ rol, cost = 1, gamma = 0.01,
                  data = entrena[,names(entrena) %in% c("clase_accidente","rol")])

modelo_svm


# Creacion de modelo KNN
library(caret)
library(class)
library(kknn)

glimpse(accidentes)
glimpse(entrena)
glimpse(prueba)
summary(accidentes)
glimpse(accidentes_knn)
anyNA(accidentes_knn)

#accidentes_knn <- accidentes[,names(accidentes) %in% c("clase_accidente","anno","edad.mean")]

corrplot::corrplot(cor(accidentes[,names(entrena) %in% c("anno.scale","edad.scale")]))

#predict.type <- knn(entrena[,names(entrena) %in% c("anno.scale","edad.scale")],
#                    prueba[,names(prueba) %in% c("anno.scale","edad.scale")],
#                    entrena$clase_accidente, k = 10 )

modelo_knn <- kknn(clase_accidente ~ rol + provincia , train = entrena, 
                   test = prueba, k = 4)


# creación del modelo NEURALNET
modelo_neuronal <- 
  nnet::nnet(clase_accidente ~ rol + sexo + provincia , data = entrena,
              size = 2, maxit = 10000, decay = 0.001, rang = 0.05, 
              na.action = na.omit, skip = TRUE)



#-------------------------------------------------------------------------------
# 4 Evaluación de los Modelos
#-------------------------------------------------------------------------------
#Random Forest
predic_bosque <- predict(modelo_bosque, newdata = prueba, type = 'class')

table(predic_bosque, prueba$clase_accidente)

# caret
confusionMatrix(data = predic_bosque, reference = prueba$clase_accidente,
                       positive = "Víctima" )

#**Evaluación del modelo Arbol de decisión**
predic_arbol <- predict(modelo_arbol, newdata = prueba, type = 'class')
table(predic_arbol ,prueba$clase_accidente )

confusionMatrix(data = predic_arbol, reference = prueba$clase_accidente,
                positive = "Víctima" )

#**Evaluación de modelo SVM**

predic_svm <- predict(modelo_svm, newdata = prueba, type = 'class')
table(predic_svm, prueba$clase_accidente)

caret::confusionMatrix(data = predic_svm, reference = prueba$clase_accidente,
                       positive = "Víctima" )


# Evaluación del modelo KNN
fit <- fitted(modelo_knn)

table(prueba$clase_accidente,fit)

confusionMatrix(prueba$clase_accidente, fit, positive = "Víctima")


#Evaluación del modelo de la red neuronal
pedict_neuronal <- predict(modelo_neuronal, newdata = prueba, type = "class")

table(prueba$clase_accidente, pedict_neuronal, dnn = c("Actual","Predicho"))

confusionMatrix( data = factor(pedict_neuronal), reference = prueba$clase_accidente, 
                positive = "Víctima")


length(factor(pedict_neuronal)) 
class(factor(pedict_neuronal))
class(prueba$clase_accidente) 

summary(pedict_neuronal)
summary(prueba$clase_accidente)


# prueba grafico
--------------------------------------------------------------------------------
# read in data
df <- InsectSprays

# get sum of all insects by spray
df2 <- aggregate(count ~ spray, df, sum)

# plot as a bar chart
p <- ggplot(df2, aes(x=spray, y=count)) + geom_bar(stat="identity")
p

#---------------------------------
# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()

g + geom_bar(aes(fill = drv))

#------------------------------------------
df <- data.frame(x = c("a","a","b","b"), y = 2:5, g = rep(1:2, 2))
p <- ggplot(df, aes(x, y, group = g)) +
  geom_col(position = "dodge", fill = "grey50", colour = "black")
p



