install.packages("caret")
library(caret)
library(dplyr)

source <- read.csv("Wine_Quality_Data.csv")

##################################################### DATA CLEANING ##################################################################

#Na elimination

valori_null <- is.na(source)

source_null <- source[rowSums(valori_null)>0, ] #non vengono rilevati NA

data <- source

# Eliminazione dei duplicati

duplicati <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]

#Mantieni solo la prima occorrenza di ciascun valore duplicato nella colonna
data <- data[!duplicated(data), ]

data_backup <- data #creiamo un backup

#puliamo l'environment
rm(duplicati)
rm(source_null)
rm(valori_null)
#rm(source)

################################################### DATA VISUALIZZATION ###############################################################

#Acidità fisso (Fixed acidity): g/l (grammi per litro)
hist(data$fixed_acidity, col= "#F7D451", main = "", xlab="Acidità fissa", ylab="g/l")

#Acidità volatile (Volatile acidity): g/l (grammi per litro)
hist(data$volatile_acidity, col= "#F7D451", main = "", xlab="Acidità volatile", ylab="g/l")

#Acido citrico (Citric acid): g/l (grammi per litro)
hist(data$citric_acid, col= "#F7D451", main = "", xlab="Acido citrico", ylab="g/l")

#Zucchero residuo (Residual sugar): g/l (grammi per litro)
hist(data$residual_sugar, col= "#F7D451", main = "", xlab="Zucchero residuo", ylab="g/l")

#Cloruri (Chlorides): g/l (grammi per litro)
hist(data$chlorides, col= "#F7D451", main = "", xlab="Cloruri", ylab="g/l")

#Anidride solforosa libera (Free sulfur dioxide): mg/l (milligrammi per litro)
hist(data$free_sulfur_dioxide, col= "#F7D451", main = "", xlab="Anidride solforosa libera", ylab="mg/l")

#Anidride solforosa totale (Total sulfur dioxide): mg/l (milligrammi per litro)
hist(data$total_sulfur_dioxide, col= "#F7D451", main = "", xlab="Anidride solforosa totale", ylab="mg/l")

#Densità (Density): g/cm³ (grammi per centimetro cubo)
hist(data$density, col= "#F7D451", main = "", xlab="Densità", ylab="g/cm³")

#pH: unità di pH
hist(data$pH, col= "#F7D451", main = "", xlab="pH", ylab="livello")

#Solfati (Sulphates): g/l (grammi per litro)
hist(data$sulphates, col= "#F7D451", main = "", xlab="Solfati", ylab="g/l")

#Alcol (Alcohol): % vol. (percentuale per volume)
hist(data$alcohol, col= "#F7D451", main = "", xlab="Alcol", ylab="% vol")

#Qualità (Quality): punteggio da 0 a 10
hist(data$quality, col= "#F7D451", main = "", xlab="Qualità", ylab="punteggio")

#Colore
pie(table(data$color), labels = c("Rossi", "Bianchi"), col = c("#722f37", "#f8f8ff"))

################################################# CORRELATION ANALYSIS #################################################################

encode_labels <- function(column) {
  unique_labels <- unique(column)
  labels_to_numbers <- as.numeric(factor(column, levels = unique_labels))
  return(labels_to_numbers)
}

# specie e isola (categorici) subiscono anch'essi una modifica
data$color <- as.numeric(as.factor(data$color))

cormatrix <- cor(data)
cormatrix

heatmap(cormatrix, 
        symm = TRUE,  # simmetrica
        col = colorRampPalette(c("#99D9EA", "white", "#FF7F50"))(100), 
        main = "Heatmap della correlazione tra attributi")

###################################################### UNDERSAMPLING ####################################################################

white <- data %>% filter (data$color == 2)
white_under <- white[sample(nrow(white), 2000, replace = FALSE),]

ggplot(white, aes(x = fixed_acidity)) +
  geom_density() +
  theme_classic()

ggplot(white_under, aes(x = fixed_acidity)) +
  geom_density() +
  theme_classic()
