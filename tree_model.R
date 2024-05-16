install.packages(datawizard)
install.packages(randomForest)

library(datawizard)
library(randomForest)
library(dplyr)

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

#puliamo l'environment
rm(duplicati)
rm(source_null)
rm(valori_null)
#rm(source)

#################################################### DATA VISUALIZATION ################################################################

#objid: object identifier
table(data$objid)
data <- data[,-1] #eliminiamo in quanto monovalore

#ra: Right Ascension
hist(data$ra, col="yellow", main="Right Ascension", xlab = "Value")

#dec: Declination 
hist(data$dec, col="yellow", main="Declination", xlab = "Value")

#Thuan-Gunn astronomic magnitude system
par(mfrow = c(3, 2)) # Set up the plotting region

hist(data$u, main = "u", xlab = "Value", col="yellow")
hist(data$g, main = "g", xlab = "Value", col="yellow")
hist(data$r, main = "r", xlab = "Value", col="yellow")
hist(data$i, main = "i", xlab = "Value", col="yellow")
hist(data$z, main = "z", xlab = "Value", col="yellow")

par(mfrow = c(1, 1)) # Close the plotting region

#run: Run Number 
hist(data$run, col="yellow", main="Run Number", xlab = "Value")

#rerun: Rerun Number
table(data$rerun)
data <- data[,-9] #eliminiamo in quanto monovalore

#camcol: Camera column
hist(data$camcol, col="yellow", main="Camera column", xlab = "Value")

#field: Field Number
hist(data$field, col="yellow", main="Field Number", xlab = "Value")

#specobjid: Object Identifier
hist(data$specobjid, col="yellow", main="Object Identifier", xlab = "Value")

#redshift: Final Redshift
hist(data$redshift, col="yellow", main="Final Redshift", xlab = "Value")

#plate: plate number
hist(data$plate, col="yellow", main="Plate Number", xlab = "Value")

#mjd: MJD of observation
hist(data$mjd, col="yellow", main="MJD of observation", xlab = "Value")

#fiberid: fiber ID
hist(data$fiberid, col="yellow", main="Fiber ID", xlab = "Value")

#class: object class
pie(table(data$class), labels = c("Galaxy", "Quasar", "Star"), col = c("yellow", "#f8f8ff", "purple"))

data <- cbind(data,data$class)
data <- data[,-12]
colnames(data)[names(data)== "data$class"] <- "class"

data_backup <- data #creiamo un backup

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

#################################################### STANDARDIZATION + NORMALIZATION ###################################################################

data <- as.data.frame(cbind(scale(data[,-16]),data[,16]))
colnames(data)[names(data)== "V16"] <- "class"
#data <- as.data.frame(normalize(data))
#names(data) <- names(source[,-c(1,10)])

####################################################### DECISION TREE ANALYSIS ###########################################################

set.seed(123)

ntrain <- ceiling(2*NROW(data)/3)

train <- sample(NROW(data),ntrain, replace = FALSE)
test <- setdiff(data, train)

rf <- randomForest(as.factor(class) ~ . , data = data, mtry=3, subset = train)
rf

plot(rf, col="#A20045", main="Random forest")
varImpPlot(rf, main="Variable importance", pch = 19, color="#A20045")

my.mtry <- tuneRF(data[,-16],as.factor(data$class), ntreeTry=500,
                  stepFactor=1.5,improve=0.001, trace=TRUE, plot=TRUE)

################################################
#Completare random forest variando i parametri + var imp + undersampling
#boosting + bagging
#rpart


