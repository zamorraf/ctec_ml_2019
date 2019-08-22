casas <- read.csv('kc_house_data.csv', header = T, na.strings = '?')
casas$bathrooms[1:10]

round(casas$bathrooms[1:10])

casas$bathrooms <- round(casas$bathrooms)

casas$sqft_living[is.na(casas$sqft_living)] <- median(casas$sqft_living, na.rm = TRUE)

var <- c(1,2,NA,5,6)
median(var, na.rm = TRUE)

var[is.na(var)] <- 100

feature_scaling <- function(x) {
  x_escalado <- ((x - min(x)) / (max(x) - min(x)))
  return(x_escalado)
}

feature_scaling (casas$sqft_lot)


var_ft_scale <- feature_scaling (casas$sqft_lot)
var_scale <- scale (casas$sqft_lot, scale = FALSE ,center = FALSE)
var_scale_center <- scale (casas$sqft_lot, scale = FALSE ,center = TRUE)
var_scale_scaled <- scale (casas$sqft_lot, scale = TRUE ,center = FALSE)
var_scale_zscore <- scale (casas$sqft_lot, scale = TRUE ,center = TRUE)

summary (var_ft_scale)
summary (var_scale)
summary (var_scale_center)
summary (var_scale_scaled)
summary (var_scale_zscore)

summary(casas$sqft_lot <- scale (casas$sqft_lot, scale = FALSE ,center = TRUE))

head(casas$sqft_lot, n = 20)
summary(casas$sqft_lot)




# Ejemplo
datos <- c(1,1,2,2,4,4,5,5,5,5,6,8,8,8,9,9,9,9,10,10)
summary(datos)

# z-score
scale(datos)

# datos centrados
scale(datos, center = TRUE, scale = FALSE)

scale(datos, center = FALSE, scale = TRUE)
