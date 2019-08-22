# Librerias
library(caret)
library(caTools)
library(lessR)
library(Metrics)

# Carga de datos desde el CSV
autos <- read.csv('auto-mpg_g.csv', header = TRUE)

str(autos)
names(autos)

# Correlaciones de los atributos
ggpairs(autos, 1:8)

# Se convierte la columna cyl a factor
autos$cyl <- factor(autos$cyl, levels = c(3,4,5,6,8), 
                               labels = c("3c","4c","5c","6c","8c"))


# Setea la semilla para obtener los mismos resultados cada vez que se ejecuta 
# la decision.
set.seed(2018)

# Se crea el vector para partición de datos de entrenamiento / testeo 
t.id <- createDataPartition(autos$mpg, p = 0.7, list = FALSE)

# Se crea el vector para partición de datos de entrenamiento / testeo 
t.id <- sample.split(Y = autos$origin, SplitRatio = 0.80)



# Datos de entrenamiento / prueba
autos_entrena <- subset(autos, t.id == TRUE)
autos_prueba <- subset(autos, t.id == FALSE)

# distribucion de la partición de acuerdo a la etiqueta utilizada 
table(autos$origin, t.id)

# Cantidad de elementos en las dos particiones
dim(autos_entrena)
dim(autos_prueba)

nrow(autos_entrena)

# Se crea el conjunto de datos para el modelo, se exclueyen las variables para 
# el conjunto de datos del modelo
#   7 = model.year; 
#   8 = origin (variable categórica)
#   9 = model.name (variable categórica con string que no sirve)
autos_modelo <- autos[t.id, -c(7,8,9)]


# Se crea el modelo lineal
modelo <- lm(mpg ~ ., data = autos_entrena[,-9])
summary(modelo)

# Funcion de predicción
# mpg = 37.824753 + 
#       7.157515 * 4c + 9.274319 * 5c + 3.854019 * 6c + 6.758291 * 8c 
#       - -0.001200 * disp - 0.043791 * hp - 0.005511 * weight + 0.041959 * acc


# Se crea el modelo lineal con reg
X1 <- autos_modelo$hp
X2 <- autos_modelo$weight
Y <- autos_modelo$mpg

mydata <- data.frame(X1, X2, Y)
mydata <- data.frame(autos_modelo$hp, autos_modelo$weight, autos_modelo$mpg)
reg(autos_modelo.mpg ~ autos_modelo.hp)

modelo <- reg(mpg ~ hp + weight, data = autos_entrena)

reg(Y ~ X1 + X2)

modelo$call
summary(modelo$residuals)
summary(modelo$coefficients)
summary(modelo)
class(modelo$out_predict)
class(modelo)


# calcular el mse

predict(modelo, autos_prueba)

Metrics::mse(autos_modelo$mpg, predict(modelo, autos_prueba))  





