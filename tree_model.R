install.packages("datawizard")
install.packages("randomForest")

library("datawizard")
library("randomForest")

source <- read.csv("Skyserver.csv")

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

data <- data[,-1]
data <- data[,-9]

################################################# CORRELATION ANALYSIS #################################################################

encode_labels <- function(column) {
  unique_labels <- unique(column)
  labels_to_numbers <- as.numeric(factor(column, levels = unique_labels))
  return(labels_to_numbers)
}

# specie e isola (categorici) subiscono anch'essi una modifica
data$class <- as.numeric(as.factor(data$class))

cormatrix <- cor(data)
cormatrix

heatmap(cormatrix, 
        symm = TRUE,  # simmetrica
        col = colorRampPalette(c("#99D9EA", "white", "#FF7F50"))(100), 
        main = "Heatmap della correlazione tra attributi")

#################################################### STANDARDIZATION + NORMALIZATION###################################################################

data <- scale(data)
data <- as.data.frame(normalize(data))
names(data) <- names(source[,-c(1,10)])

####################################################### DECISION TREE ANALYSIS ###########################################################

ntrain <- ceiling(2*NROW(data)/3)

train <- sample(NROW(data),ntrain)

rf <- randomForest(as.factor(class) ~ . , data = data , subset = train)
rf

plot(rf, col="#A20045", main="Random forest")
varImpPlot(rf, main="Variable importance", pch = 19, color="#A20045")

