library(dplyr)

# 1. Carga del archivo
# ------------------------------------------------------------------------------

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
agricus_lepiota <- read.table(url, header = FALSE,  sep = ",", stringsAsFactors = FALSE)
agricus_lepiota <- read.csv("agaricus-lepiota.data", header = FALSE)
#write.table(mushroomsDB, file = "Mushroom.csv", sep = ",")


# Carga de los nombres de columnas
nombres_columna <- c("class","cap_shape","cap_surface","cap_color","bruises",
                     "odor","gill_attachment","gill_spacing","gill_size", 
                     "gill_color","stalk_shape","stalk_root",
                     "stalk_surface_above_ring","stalk_surface_below_ring",
                     "stalk_color_above_ring","stalk_color_below_ring",
                     "veil_type","veil_color","ring_number","ring_type",
                     "spore_print_color","population","habitat")

names(agricus_lepiota) <- nombres_columna

# Etiquetas de los factores
agricus_lepiota$class <- factor(agricus_lepiota$class,
                                levels = c("p","e"),
                                labels = c("poisonous","edible"))


# Estadisticas de las datos
dim(agricus_lepiota)
glimpse(agricus_lepiota)
class(agricus_lepiota)

table(agricus_lepiota$class)

visdat::vis_dat(agricus_lepiota)




# 2. Entendimiento de los datos
# ------------------------------------------------------------------------------