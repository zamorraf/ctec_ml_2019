library(dplyr)
library(readr)

charset <- guess_encoding("temp_5571830814335439232.csv" )

charset

accidentes <- read.csv("temp_5571830814335439232.csv", encoding = "UTF-8", 
                       header = TRUE)



glimpse(accidentes)
summary(accidentes)
head(accidentes)

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


accidentes <- accidentes[,-1]
