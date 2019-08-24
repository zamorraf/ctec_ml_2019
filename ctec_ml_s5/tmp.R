library(dplyr)

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
agricus_lepiota <- read.table(url, header = FALSE,  sep = ",", stringsAsFactors = FALSE)
#write.table(mushroomsDB, file = "Mushroom.csv", sep = ",")

dim(agricus_lepiota)
glimpse(agricus_lepiota)

class(agricus_lepiota)


nombres_columna <- c("cap_shape","cap_shape","cap_surface","cap_color","bruises",
                     "odor","gill_attachment","gill_spacing","gill_size", 
                     "gill_color","stalk_shape","stalk_root",
                     "stalk_surface_above_ring","stalk_surface_below_ring",
                     "stalk_color_above_ring","stalk_color_below_ring",
                     "veil_type","veil_color","ring_number","ring_type",
                     "spore_print_color","population","habitat")

names(agricus_lepiota) <- nombres_columna
