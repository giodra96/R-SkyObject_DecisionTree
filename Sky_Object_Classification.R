
# Titolo: La Data Science applicata all'astronomia: un approccio statistico alla classificazione dei corpi celesti usando gli alberi decisionali

######################################################################################################################################
# Introduzione
######################################################################################################################################

# L'acronimo SDSS sta per Sloan Digital Sky Survey. Si tratta di un ambizioso progetto astronomico durato vent'anni, durante il quale sono stati
# raccolti dati circa la luminosità, le onde elettromagnetiche, il movimento degli oggetti celesti, attraverso l'utilizzo di telescopi,
# fotocamere digitali, spettrografi.

# Tutti questi dati sono contenuti all'interno del database pubblico SDSS SkyServer. Questi dati sono stati resi liberamente accessibili alla
# comunità scientifica e al pubblico in generale. Per concludere, lo Sloan Digital Sky Survey è stato un progetto rivoluzionario che ha avuto un
# impatto significativo sulla nostra comprensione dell'universo e ha reso disponibili vasti quantitativi di dati astronomici alla comunità scientifica globale.

# Sono stati proprio questi dati l'oggetto del presente studio, che, in particolare, si propone di classificare i corpi celesti contenuti nel
# database nelle tre categorie di stella, galassia o quasar, sulla base di tutte le informazioni raccolte durante l’osservazione del cielo.

#######################################################################################################################################
# Importazione e preprocessamento
#######################################################################################################################################

install.packages("datawizard")
install.packages("randomForest")
install.packages("dplyr")
install.packages("caret")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("patchwork") # per inserire i grafici nella stessa schermata in una griglia
install.packages("partykit")
install.packages("rpart")
install.packages("rpart.plot") # visualizzazione degli alberi
install.packages("gbm") # boosting
install.packages("pdp")
install.packages("ipred") #bagging
install.packages("purrr") #ROC curve
install.packages("pROC")
install.packages("PRROC")

library(datawizard)
library(randomForest)
library(dplyr)
library(caret)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(partykit)
library(rpart)
library(rpart.plot)
library(gbm)
library(pdp)
library(ipred)
library(purrr)
library(pROC)
library(PRROC)

source <- read.csv("Skyserver.csv")

# eliminazione valori nulli
source_null <- source[rowSums(is.na(source))>0, ] #non vengono rilevati NA
data <- source

# Eliminazione dei duplicati
duplicati <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]

# si mantiene solo la prima occorrenza di ciascun duplicato
data <- data[!duplicated(data), ]

# pulizia dell'environment
rm(duplicati)
rm(source_null)

#######################################################################################################################################
# Data visualization: grafici e osservazione preliminare del dataset
#######################################################################################################################################

# prima variabile oggetto di osservazione è il tipo di corpo celeste, caratteristica sulla base della quale verrà effettuata classificazione

# cambio dell'etichetta "QSO" in "QUASAR" per consistenza con le altre due classi:
# selezione righe corrispondenti all'etichetta QSO
qso_rows <- data[data$class == "QSO", ]
# cambio di etichetta
qso_rows$class <- "QUASAR"
# aggiornamento del dataset di partenza con le righe aggiornate
data <- rbind(data[data$class != "QSO", ], qso_rows)
remove(qso_rows)

table(data$class)
pie(table(data$class), labels = c("Galaxy", "Quasar", "Star"), col = c("lightblue", "blue", "black"))

# codice identificativo del corpo celeste
table(data$objid) # tutte gli oggetti nel dataset hanno lo stesso codice

# si tenga presente che questo dataset è stato estrapolato da uno più grande, contenente anche oggetti di diversa natura (nebulose, asteroidi, stelle brune, stelle pulsanti, agnelle...)
# si deduce, visto l'unico valore in questa colonna, che i tre oggetti di stelle, quasar e galassie facciano parte della stessa categoria secondo il criterio con cui è stato assegnato questo codice
# di conseguenza viene eliminata l'informazione contenuta in questa variabile

data <- data[,-1]  # eliminazione della colonna

# L'ascensione retta e la declinazione sono coordinate utilizzate nel sistema di coordinate equatoriali per determinare la posizione di un corpo celeste sulla sfera celeste,
# simile a come la latitudine (decl) e la longitudine (ascens) sono utilizzate per localizzare punti sulla superficie terrestre rispettivamente.

# ra: Right Ascension (ascensione retta): misura l'angolo lungo l'equatore celeste a partire dal punto vernale. Espresso in ore, minuti e secondi.
# dec: Declination (declinazione): misura l'angolo sopra o sotto l'equatore celeste. Espresso in gradi.

# si possono visualizzare stelle, galssie e quasar nello spazio utilizzando un piano cartesiano e queste coordinate:
celestial_data <- data[c(1:20),c(1,2,13)] # vengono selezionati solo 20 righe per rendere il grafico leggibile

# bisogna portare ra e dec alla stessa unità di misura, gradi: 1 ora di RA = 15 gradi
celestial_data$ra <- celestial_data$ra * 15

ggplot(celestial_data, aes(x = ra, y = dec, label=class)) +
  geom_point(color = "black", size = 1.5) +
  geom_text(vjust = -1, hjust = 1.2) +
  labs(title = "",
       x = "Right Ascension (degrees)",
       y = "Declination (degrees)") +
  theme_minimal()

# Il sistema di magnitudine astronomica Thuan-Gunn è un sistema fotometrico utilizzato in astronomia per misurare la luminosità delle stelle e di altri corpi celesti.
# È progettato per osservazioni nella banda ottica, utilizzando specifici filtri per isolare determinate bande di lunghezza d'onda della luce
# (luce ultravioletta, infrarossa, verde, rossa e luce visibile). In questo modo si può ottenere una misura precisa della magnitudine (cioè, della luminosità) di un oggetto.

celestial_data <- cbind(celestial_data, data[c(1:20),c(3,4,5,6,7)]) # vengono aggiunti i dati sui cinque filtri allo stesso sottoinsieme di 20 corpi celesti

#######################################################################################################################################

# banda ultravioletta
ggplot(celestial_data, aes(x = ra, y = dec)) +
  geom_point(aes(color = u), size = 3) +
  scale_color_gradient(low = "blue", high = "cyan", name = "Brightness") +
  labs(title = "Ultraviolet spectrum (Thuan-Gunn system)",
       x = "Right Ascension (degrees)",
       y = "Declination (degrees)") +
  theme_minimal()

# boxplot banda ULTRAVIOLETTA
ggplot(data, aes(x = class, y = u, fill = class)) +
  geom_boxplot() +
  labs(title = "",
       x = "Class of celestial object",
       y = "Ultraviolet spectrum") +
  theme_minimal() +
  scale_fill_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue"))

# una sola iterazione nell'eliminazione degli outliers
boxplot_values <- boxplot.stats(data$u)$out
data <- data[!data$u %in% boxplot_values, ]

# banda verde
ggplot(celestial_data, aes(x = ra, y = dec)) +
  geom_point(aes(color = g), size = 3) +
  scale_color_gradient(low = "#008000", high = "#7FFF00", name = "Brightness") +
  labs(title = "Green spectrum (Thuan-Gunn system)",
       x = "Right Ascension (degrees)",
       y = "Declination (degrees)") +
  theme_minimal()

# boxplot banda VERDE
ggplot(data, aes(x = class, y = g, fill = class)) +
  geom_boxplot() +
  labs(title = "",
       x = "Class of celestial object",
       y = "Green spectrum") +
  theme_minimal() +
  scale_fill_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue"))

# una sola iterazione nell'eliminazione degli outliers
boxplot_values <- boxplot.stats(data$g)$out
data <- data[!data$g %in% boxplot_values, ]

# banda rossa
ggplot(celestial_data, aes(x = ra, y = dec)) +
  geom_point(aes(color = r), size = 3) +
  scale_color_gradient(low = "#8B0000", high = "#FF6347", name = "Brightness") +
  labs(title = "Red spectrum (Thuan-Gunn system)",
       x = "Right Ascension (degrees)",
       y = "Declination (degrees)") +
  theme_minimal()

# boxplot banda ROSSA
ggplot(data, aes(x = class, y = r, fill = class)) +
  geom_boxplot() +
  labs(title = "",
       x = "Class of celestial object",
       y = "Red spectrum") +
  theme_minimal() +
  scale_fill_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue"))

# una sola iterazione nell'eliminazione degli outliers
boxplot_values <- boxplot.stats(data$r)$out
data <- data[!data$r %in% boxplot_values, ]

# banda infrarossa
ggplot(celestial_data, aes(x = ra, y = dec)) +
  geom_point(aes(color = i), size = 3) +
  scale_color_gradient(low = "#FF4500", high = "#FFA500", name = "Brightness") +
  labs(title = "Infrared spectrum (Thuan-Gunn system)",
       x = "Right Ascension (degrees)",
       y = "Declination (degrees)") +
  theme_minimal()

# boxplot banda INFRAROSSA
ggplot(data, aes(x = class, y = i, fill = class)) +
  geom_boxplot() +
  labs(title = "",
       x = "Class of celestial object",
       y = "Infrared spectrum") +
  theme_minimal() +
  scale_fill_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue"))

# una sola iterazione nell'eliminazione degli outliers
boxplot_values <- boxplot.stats(data$i)$out
data <- data[!data$i %in% boxplot_values, ]

#banda della luce visibile
ggplot(celestial_data, aes(x = ra, y = dec)) +
  geom_point(aes(color = z), size = 3) +
  scale_color_gradient(low = "#800080", high = "#DA70D6", name = "Brightness") +
  labs(title = "Visible light spectrum (Thuan-Gunn system)",
       x = "Right Ascension (degrees)",
       y = "Declination (degrees)") +
  theme_minimal()

# boxplot banda DELLA LUCE VISIBILE
ggplot(data, aes(x = class, y = z, fill = class)) +
  geom_boxplot() +
  labs(title = "",
       x = "Class of celestial object",
       y = "Visible light spectrum") +
  theme_minimal() +
  scale_fill_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue"))

# una sola iterazione nell'eliminazione degli outliers
boxplot_values <- boxplot.stats(data$z)$out
data <- data[!data$z %in% boxplot_values, ]

# pulizia dell'environment
remove(boxplot_values)
remove(celestial_data)

#######################################################################################################################################

# A seguire vengono descritti gli attributi run, camcol, field e rerun.
# Sono tutti usati per descrivere ed identificare le immagini dei corpi celesti "scattate" durante la raccolta dei dati sui corpi celesti del presente dataset.
# Ancora più precisamente, tutte fanno riferimento ad uno specifico "field", ossia una specifica sezione di un'intera immagine (corrispondente a 2048 per 1489 pixel)
# Si tenga bene a mente come ciascuna immagine richieda diverse scansioni prima di essere esaustiva, e persino ciascun field della stessa richiede più scansioni
# effettuate con gli spettroscopi ottici. Queste scansioni sono note come "run".

# run: codice identificativo della specifica scansione effettuata
# camcol: "CAMera COLumn", un numero tra 1 e 6 che identifica un setting impostato durante la scansione
# field: codice identificativo dello specifico field nell'immagine
# rerun: codice addizionale che specifica la modalità con cui è stata processata l'immagine. in particolare, in questo sottoinsieme di dati estrapolato dal database originale,
# tutte le immagini sono state processate con lo stesso metodo e dunque è presente un solo valore per questo attributo 

table(data$rerun)
data <- data[,-9] # la colonna viene eliminata

# le ultime variabili da esaminare sono plate, fiberID, MJD (Modified Julian Date) e redshift.

# Plate: per ogni esposizione spettroscopica si utilizza un grande e sottile piatto metallico circolare che posiziona le fibre ottiche nel piano focale del telescopio. Queste fibre poi alimentano gli spettrografi. Ciascun piatto ha un numero seriale unico.

# Fiber ID: ciascun corpo celeste ha assegnato un ID relativo alla sovracitata fibra ottica utilizzata per l'esposizione spettroscopica.

# La "Data Modificata di Julian", è il sistema di datazione astronomico utilizzato nella raccolta dei dati astronomici, inclusi immagini e spettri raccolti da telescopi e strumenti astronomici.

# In fisica, il "redshift" avviene quando la luce o altre radiazioni elettromagnetiche provenienti da un corpo celeste aumentano di lunghezza d'onda,
# spostandosi verso la banda rossa dello spettro elettromagnetico. Nel caso di questo studio, visti i valori compresi tra -0,5 e 5 approssimativamente,
# si intuisce si stia facendo riferimento al redshift cosmologico, ossia quell'aumento della lunghezza d'onda delle onde (sia luminose che elettromaghetiche)
# emesse dai corpi celesti causato dal loro allontanamento dalla terra, dovuto all'espansione dell'universo.

# sulla base del redshift, si può dedurre la distanza del corpo celeste dalla terra.

# Redshift positivo: indica che l'oggetto si sta allontanando dalla terra, e la luce è spostata verso lunghezze d'onda maggiori (spostamento verso il rosso).
# Redshift zero: Indica che non c'è spostamento della lunghezza d'onda; l'oggetto è statico rispetto alla terra.
# Redshift negativo, o "blueshift": Indica che l'oggetto si sta avvicinando alla terra, e la luce è spostata verso lunghezze d'onda minori (spostamento verso il blu).

hist(data$redshift, col="#99D9EA", main="Redshift distribution of celestial objects", xlab = "Redshift", ylab="")

# si osservi come la maggior parte dei corpi celesti nel dataset abbia redshift tra 0 e 0,5.
# dall'istogramma sembrano essere presenti alcuni outliers (la coda tra i valori di redshift 2 e 5 è molto piatta)

# boxplot redshift
ggplot(data, aes(x = class, y = redshift, fill = class)) +
  geom_boxplot() +
  labs(title = "",
       x = "Class of celestial object",
       y = "Redshift") +
  theme_minimal() +
  scale_fill_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue"))

# le osservazioni individuate automaticamente dal boxplot come outliers sono 781.
boxplot_values <- boxplot.stats(data$redshift)$out
length(boxplot_values)

# si noti come si tratti quasi esclusivamente di corpi celesti di tipo quasar!
table((data[data$redshift %in% boxplot_values, ])$class)

# il taglio sarebbe eccessivo e penalizzante per la classe "quasar".
remove(boxplot_values)

# Si opta per un taglio manuale in corrispondenza di redshift superiori al 3.5.
# Si tratta di sole tre osservazioni, ma si procede comunque alla rimozione
# per consentire una visualizzazione più nitida delle restanti osservazioni nei grafici
nrow(subset(data, redshift >= 3.5))
data <- subset(data, redshift <= 3.5)

# Si osservino più in dettaglio i range di redshift 

x <- subset(data, class == "STAR")
y <- subset(data, class == "GALAXY")
w <- subset(data, class == "QUASAR")

par(mfrow = c(2,2)) # inizializzazione griglia 2x2
h1 <- hist(x$redshift, col="cyan", main="Stars", xlab = "Redshift", ylab="")
h2 <- hist(y$redshift, col="lightblue", main="Galaxies", xlab = "Redshift", ylab="")
h3 <- hist(w$redshift, col="blue", main="Quasars", xlab = "Redshift", ylab="")
par(mfrow = c(1, 1)) # ritorno alla singola cella di default

# risulta già evidente a questo punto dell'analisi come il redshift sia determinante nella classificazione:
# mentre stelle e galassie non superano valori di redshift intorno allo 0.8, i quasars hanno valori molto variabili.

# grafici a violino colorati in base al corpo celeste e riferiti alla distribuzione del redshift

v1 <- ggplot(x, aes(x = class, y = redshift)) +
  geom_violin(trim = FALSE) +
  labs(title = "",
       x = "", y = "Redshift") +
  geom_boxplot(width=0.3,  fill="black") + 
  theme_minimal()

v2 <- ggplot(y, aes(x = class, y = redshift)) +
  geom_violin(trim = FALSE) +
  labs(title = "",
       x = "", y = "Redshift") +
  geom_boxplot(width=0.3,  fill="lightblue") + 
  theme_minimal()

v3 <- ggplot(w, aes(x = class, y = redshift)) +
  geom_violin(trim = FALSE) +
  labs(title = "",
       x = "", y = "Redshift") +
  geom_boxplot(width=0.3,  fill="blue") + 
  theme_minimal()

grid.arrange(
  v1 + facet_wrap(~ class),
  v2 + facet_wrap(~ class),
  v3 + facet_wrap(~ class),
  nrow = 1, ncol = 3
)

remove(h1, h2, h3, x, y, w, v1, v2, v3)
par(mfrow = c(1, 1)) # ritorno alla singola cella di default per stampare i grafici

# si osservi ora la distribuzione delle osservazioni in relazione al redshift:
# viene confrontato il redshift con la banda r perché si trova nella parte rossa dello
# spettro e quindi è più adatta per studiare oggetti con redshift diversi

ggplot(data, aes(x = redshift, y = r)) + 
  geom_point(color = "#800020") +
  labs(title = "",
       x = "Redshift",
       y = "r-band Magnitude") +
  theme_minimal()

# Sull'asse delle ascisse "redshift" rappresenta lo spostamento verso il rosso della luce emessa dagli oggetti celesti.
# Sull'asse delle ordinate la magnitudine Thuan-Gunn nella banda "r", che rappresenta la luminosità apparente
# degli oggetti celesti nella banda di lunghezza d'onda del rosso.

# Dunque, dove si trovano rispettivamente, stelle, galassie e quazar in questa distribuzione?

g1 <- ggplot(data, aes(x = redshift, y = r, color = class)) +
  geom_point(size = 1) +
  labs(title = "",
       x = "Redshift",
       y = "r-band Magnitude") +
  scale_color_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue")) +
  theme_minimal()

g1

# scatterplot delle altre bande di lunghezze d'onda 

g2 <- ggplot(data, aes(x = redshift, y = u, color = class)) +
  geom_point(size = 1) +
  labs(title = "",
       x = "Redshift",
       y = "Ultraviolet Magnitude") +
  scale_color_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue")) +
  theme_minimal()

g3 <- ggplot(data, aes(x = redshift, y = g, color = class)) + 
  geom_point(size = 1) +
  labs(title = "",
       x = "Redshift",
       y = "Green Magnitude") +
  scale_color_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue")) +
  theme_minimal()

g4 <- ggplot(data, aes(x = redshift, y = i, color=class)) + 
  geom_point(size = 1) +
  labs(title = "",
       x = "Redshift",
       y = "Infrared Magnitude") +
  scale_color_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue")) +
  theme_minimal()

g5 <- ggplot(data, aes(x = redshift, y = z, color=class)) + 
  geom_point(size = 1) +
  labs(title = "",
       x = "Redshift",
       y = "Visible Light Magnitude") +
  scale_color_manual(values = c("STAR" = "black", "GALAXY" = "lightblue", "QUASAR" = "blue")) +
  theme_minimal()

(g1 | g2 | g3) / (g4 | g5)

# Stelle
# Redshift: Le stelle della Via Lattea hanno redshift molto bassi, vicini a zero, poiché si trovano a distanze relativamente piccole rispetto alla terra,
# Magnitudine: La magnitudine apparente delle stelle può variare notevolmente, ma in generale si trovano stelle luminose (magnitudine bassa) e vicine.
# Nel grafico, le stelle si troverebbero generalmente nella parte sinistra (redshift basso) e la loro posizione verticale (magnitudine) potrebbe essere
# distribuita equamente, poiché dipenderà dalla loro luminosità apparente.

# Galassie
# Redshift: Le galassie possono avere una gamma di redshift più ampia, con valori che aumentano man mano che si trovano più lontano da noi.
# Magnitudine: La magnitudine apparente delle galassie può variare, ma in media sono meno luminose delle stelle vicine.
# Le galassie si troverebbero distribuite più a destra delle stelle nel grafico, con redshift che variano da bassi a medi e magnitudini che variano in
# un intervallo più ampio.

# Quasar
# Redshift: I quasar sono nuclei galattici attivi estremamente luminosi e spesso hanno redshift elevati, poiché si trovano a grandi distanze cosmiche.
# Magnitudine: Nonostante la loro distanza, i quasar possono avere magnitudini apparenti relativamente basse (cioè essere luminosi) grazie alla loro
# elevata luminosità intrinseca. I quasar si troverebbero nella parte destra del grafico, con redshift elevati, e possono avere magnitudini apparenti
# basse (luminosi) o alte (meno luminosi), a seconda della distanza e delle condizioni di osservazione.

remove(g1, g2, g3, g4, g5)

# ordinamento delle colonne
data <- cbind(data,data$class)
data <- data[,-12]
colnames(data)[names(data)== "data$class"] <- "class"

# creazione dataset di backup 
data_backup <- data

#######################################################################################################################################
# Correlazioni
#######################################################################################################################################

encode_labels <- function(column) {
  unique_labels <- unique(column)
  labels_to_numbers <- as.numeric(factor(column, levels = unique_labels))
  return(labels_to_numbers)
}

data$class <- as.numeric(as.factor(data$class))
cormatrix <- cor(data)
cormatrix

heatmap(cormatrix, 
        symm = TRUE,  # TRUE la rende simmetrica
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Heatmap of correlation matrix")

# si eliminano le variabili a bassa correlazione e quelle irrilevanti come le coordinate o le date
# (correlate maggiormente allo strumento utilizzato per l'analisi che non all'oggetto di interesse dell'analisi)

data <- data[,-c(1,2,8,9,10,11,13,14,15)]

cormatrix <- cor(data)
heatmap(cormatrix, 
        symm = TRUE,  # TRUE la rende simmetrica
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        main = "Heatmap Heatmap of correlation matrix (reduced)")

# si noti la correlazione della classe con la banda di lunghezza d'onda dell'ultravioletta in particolare,
# e poi tra redshift e green 

remove(encode_labels)
remove(cormatrix)

# Preparazione agli alberi decisionali e standardizzazione

data <- data_backup[,-c(1,2,8,9,10,11,13,14,15)]
data <- as.data.frame(cbind(scale(data[,-7]),data[,7]))
colnames(data)[names(data)== "V7"] <- "class"

# 1 = galaxy, 2 = qso, 3 = star

data$class<-as.factor(data$class)
data$redshift<-as.numeric(data$redshift)
data$u<-as.numeric(data$u)
data$g<-as.numeric(data$g)
data$r<-as.numeric(data$r)
data$i<-as.numeric(data$i)
data$z<-as.numeric(data$z)

str(data)

#######################################################################################################################################
# Partition of dataset into test-set and training-set
#######################################################################################################################################

set.seed(123)
train_rows <- createDataPartition(data$class, p = 0.7, 
                                  list = FALSE, 
                                  times = 1)
# training set

train <-data[train_rows,]
ntrain_rows<-nrow(train)
table(train$class)
# percentuale della distribuzione delle osservazioni nelle tre classi del train:
round(table(train$class)/ntrain_rows*100,1) 

# test set

test<-data[-train_rows,]
ntest_rows<-nrow(test)
table(test$class)
# percentuale della distribuzione delle osservazioni nelle tre classi del test:
round(table(test$class)/ntest_rows*100,1)

remove(train_rows,ntest_rows,ntrain_rows) #pulizia dell'environment

####################################################################################################
# Pacchetto rpart
####################################################################################################

set.seed(132)
# Addestramento del modello sul training-set e visualizzazione dei risultati
tree_model_rpart <- rpart(class ~ ., data = train, method = "class")
summary(tree_model_rpart)
print(tree_model_rpart)

# L'errore relativo diminuisce sensibilmente già al primo split. Il modello è in grado di classificare 
# in maniera molto accurata grazie alle variabili che ha a disposizione. In particolare il redshift è
# determinante ed è anche la variabile più importante come indicato nel summary del modello.

# Anche i risultati relativi agli split principali e surrogati confermano tale osservazione,
# poiché il mdoello continua a prendere il redshift come riferimento anche negli split successivi.

# Il nodo radice ha una divisione significativa basata sulla variabile redshift, migliorando notevolmente il criterio di impurità.
# Le ulteriori suddivisioni rafforzano la separazione delle classi, con ogni nodo foglia che mostra chiara predominanza di una classe particolare.

# Visualizzazione grafica dell'albero
rpart.plot(tree_model_rpart) # 1 = galaxy, 2 = qso, 3 = star
plotcp(tree_model_rpart) 

# dal grafico si evince che il valore di CP che minimizza l'errore relativo è pari a 0.04.
# il complexity parameter è un indicatore della complessità dell'albero generato, sulla base del suo numero di foglie (e quindi numero di split)
# valori più alti = alberi più semplici
# Si ripete la generazione dell'albero imponedo CP= 0.04

tree_model_rpart <- rpart(class ~ ., data = train, method = "class", control = rpart.control(cp = 0.04))
summary(tree_model_rpart)
print(tree_model_rpart)

# Visualizzazione grafica dell'albero
rpart.plot(tree_model_rpart) # 1 = galaxy, 2 = qso, 3 = star

# Nonostante l'inserimento del valore di CP ottimale, il risultato non si è discostato dal precedente.
# Questo indica che il modello, anche senza quest'ultiima ottimizzazione, è in grado già di classificare bene i dati.

# Previsioni sul test-set e valutazione del modello
rpart_predictions <- predict(tree_model_rpart, test, type = "class")

# Statistiche sulla predizione 

confusionMatrix(rpart_predictions,test$class)

# Il modello ha:
# accuracy (capacità di previsioni corrette sul totale delle osservazioni) pari all'98,82%
# sensitività (capacità del modello di identificare correttamente i positivi): GALAXY 98,71%, QUASAR 93,90%, STAR 100%  
# specificità (Capacità del modello di identificare correttamente i negativi): GALAXY 98,93%, QUASAR 99,58%, STAR 99,53% 

remove (rpart_predictions)

####################################################################################################
# Pacchetto partykit (ctree) - conditional inference trees
####################################################################################################

set.seed(132)
ctree_model <- ctree(class ~ ., data = train, minsplit = 150, mincriterion=0.95)
summary(ctree_model)
plot(ctree_model)

# Il grafico mostra come vengono splittate le osservazioni sulla base di più variabili (non più esclusivamente il redshift).
# L'istogramma nei nodi foglia rappresenta la classificazione delle osservazioni nelle tre classi iniziali
# (1 = galaxy, 2 = qso, 3 = star)

# Previsioni sul test-set e valutazione del modello
ctree_predictions <- predict(ctree_model, test)

# Statistiche sulla predizione 

confusionMatrix(ctree_predictions,test$class)
# Il modello ha:
# accuracy pari all'98,82%
# sensitività: GALAXY 98,71%, QUASAR 93,90%, STAR 100%  
# specificità: GALAXY 98,93%, QUASAR 99,58%, STAR 99,53% 

remove(ctree_predictions)

####################################################################################################
# bagging con il pacchetto ipred
####################################################################################################

set.seed(132)
bag_model <- bagging(class ~ ., train, nbag=50, coob=TRUE, control = rpart.control(minsplit = 10, cp = 0.001, min_depth=2) )
bag_model

# Previsioni sul test set e valutazione del modello 
test_pred <- predict(bag_model, test)

# Statistiche sulla predizione 

confusionMatrix(test_pred,test$class)
# Il modello ha:
# accuracy pari all'98,85%
# sensitività: GALAXY 98,71%, QUASAR 94,31%, STAR 100%  
# specificità: GALAXY 99,00%, QUASAR 99,58%, STAR 99,53% 


# Calcolo della variable importance 
# Funzione per estrarre l'importanza delle variabili da un singolo albero
get_importance <- function(model) {
  importance <- model$variable.importance
  return(importance)
}

# Estrazione delle importanze dai singoli alberi nel modello di bagging
tree_importances <- lapply(bag_model$mtrees, function(x) get_importance(x$btree))

# Combinazione delle variable importance in una singola matrice, riempiendo i NA con 0
importance_matrix <- do.call(rbind, lapply(tree_importances, function(x) {
  imp <- numeric(length(bag_model$mtrees[[1]]$btree$variable.importance))
  names(imp) <- names(bag_model$mtrees[[1]]$btree$variable.importance)
  imp[names(x)] <- x
  return(imp)
}))

average_importance <- colMeans(importance_matrix, na.rm = TRUE)
importance_df <- data.frame(
  Variable = names(average_importance),
  Importance = average_importance
)

# Creazione del data frame per il plotting
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Plotting
importance_plot <- ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Variable Importance in Bagging Model",
       x = "Variables",
       y = "Importance") +
  theme_minimal()

# Salvare il grafico in un file
ggsave("bagging_importance_plot.png", importance_plot)

remove(test_pred)

####################################################################################################
# gbm (Gradient Boosting Machines)
####################################################################################################

set.seed(132)
gbm_model <- gbm(class~ ., train, distribution = "multinomial", n.trees = 200, interaction.depth = 1, shrinkage=0.1, cv.folds=5)

# ricerca del numero ottimale di alberi
best<-gbm.perf(gbm_model, method = "cv", plot.it = TRUE) 
best # il numero si avvicina molto al 200 imposto nell'allenamento del modello

# variazioni all'algoritmo gbm

# interaction.depth per specificare la profondità massima di ogni albero. Default 1. Aumentandolo,
# l'accuratezza sale, molto vicina al massimo della random forest. Con valore 3 peggiora leggermente.

# n.minobsinnode numero minimo di osservazioni nei nodi foglia
# si impone almeno 20 e l'accuratezza resta 0.988

# shrinkage
# default=0.1. Per valori più piccoli, gli split sono più conservatori.
# Con numeri molto piccoli si rischia overfitting e con numeri troppo grandi si rischia poca accuratezza.

# Visualizzazione del modello
summary(gbm_model) # con allegato grafico dell'importanza relativa delle variabili 

# 1 = galaxy, 2 = qso, 3 = star

g1<-partial(gbm_model, "redshift", n.trees=200, which.class = 1, plot = TRUE, prob = TRUE)
g1 # galassie
g2<-partial(gbm_model, "redshift", n.trees=200, which.class = 2, plot = TRUE, prob = TRUE)
g2 # quasar
g3<-partial(gbm_model, "redshift", n.trees=200, which.class = 3, plot = TRUE, prob = TRUE)
g3 # stelle

grid.arrange(
  g1, g2, g3,
  nrow = 1, ncol = 3
)

# Previsioni
gbm_predictions <- predict(gbm_model, test, n.trees = 200, type = "response")
gbm_predictions <- apply(gbm_predictions, 1, which.max)

# Conversione di gbm_predictions in factor con gli stessi livelli di test$class

class_levels <- levels(test$class)
gbm_predictions <- factor(gbm_predictions, levels = 1:length(class_levels), labels = class_levels)

# Verifica della struttura dei dati 
print(str(gbm_predictions))
print(str(test$class))

# Calcola la matrice di confusione
conf_matrix <- confusionMatrix(gbm_predictions, test$class)

# Stampa la matrice di confusione
print(conf_matrix)

remove(best, bag_probabilities, class_levels)

#####################################################################################################
# Random Forest
#####################################################################################################

set.seed(132)
# ottimizzazione dell'mtry
my.mtry <- tuneRF(train[,-7],train$class,
                  stepFactor=2,improve=0.001, trace=TRUE, plot=TRUE)

# il valore di mtry che minimizza l'errore out of bag è pari a 4. si procede in questa direzione:
rf <- randomForest(class ~ . , data = train, ntree=500, mtry = 4, iter=6)
rf

plot(rf, col="darkgreen", main="Random forest")

# l'errore converge velocemente e sembra piuttosto stabile, generare 500 alberi non porta nessun beneficio marginale. A valle di ciò
# si possono usare 150 alberi, che dal plot precedente sembra essere un numero adatto per tutte e tre le classi di errore. 

rf <- randomForest(as.factor(class) ~ . , data = train, ntree=150, mtry = 4, iter=6)
rf

# Previsioni sul test set e valutazione del modello
rf_predictions <- predict(rf, test, type = "class")

# Statistiche sulla predizione 

confusionMatrix(rf_predictions,test$class)
# Il modello ha:
# accuracy pari all'99,23%
# sensitività: GALAXY 98,18%, QUASAR 93,94%, STAR 100%  
# specificità: GALAXY 99,29%, QUASAR 99,62%, STAR 99,88% 

# importanza relativa delle variabili
varImpPlot(rf, main="Variable importance", pch = 19, color="darkgreen")

# indice di Gini: misura di importanza delle variabili utilizzate, per valutare il contributo di ciascuna nel processo di
# classificazione o regressione del modello. Come si può notare, il redshift ha valore massimo tra tutte.

# esperimento per valutare l'importanza relativa degli attributi se non fosse stata presente alcuna informazione sul redshift.
# eliminazione di tale variabile:

data_dec <- data[,-6]
train_dec <- train[,-6]
test_dec <- test[,-6]
glimpse(data_dec)

# ottimizzazione dell'mtry
set.seed(132)
my.mtry_dec <- tuneRF(train_dec[,-6],train_dec$class,
                      stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE)

# applicazione della random forest e confronto della variabile importance con la anteriore
rf_dec <- randomForest(class ~ . , data = train_dec, ntree=150, mtry = 4, iter=6)
rf_dec

# variable importance
par(mfrow = c(1,2)) # inizializzazione griglia 2x1
varImpPlot(rf_dec, main="NO redshift", pch = 19, color="darkred")
v_i2 <- varImpPlot(rf, main="YES redshift", pch = 19, color="darkgreen")

plot(rf_dec, col="darkred", main="NO redshift")
plot(rf, col="darkgreen", main="YES redshift")
par(mfrow = c(1, 1)) # ritorno alla singola cella di default

# Dal punto di vista dell'errore nella classificazione, l'assenza del redshift causa una riduzione di accuratezza.

# Previsioni sul test set e valutazione del modello 
rf_predictions2 <- predict(rf_dec, test_dec, type = "class") # 1 = galaxy, 2 = qso, 3 = star

# Statistiche sulla predizione

confusionMatrix(rf_predictions2,test$class)

# Il modello ha:
# accuracy pari all'93,22%
# sensitività: GALAXY 93,13%, QUASAR 90,65%, STAR 93,87%  
# specificità: GALAXY 95,30%, QUASAR 98,78%, STAR 94,35% 

remove(rf_predictions, rf_predictions2, rf_dec, my.mtry, my.mtry_dec, data_dec, test_dec, train_dec, v_i2)

# Anche senza redshift il livello di accuratezza è relativamente alto. C'è comunque una riduzione, in particolare
# l'errore maggiore avviene nella classificazione delle stelle. Questo ha senso sopratutto se si osserva il grafico
# della distribuzione del redshift: era proprio il redshift la variabile che determinava una distinzione chiara
# delle stelle dalle altre due classi. Di conseguenza non sorprende che la riduzione di accuratezza,
# seppur minima, si verifichi proprio in corrispondenza della classe 3.

# Al netto di questo, anche senza redshift la precisione dell'algoritmo è verosimile su star e galaxy.
# Il Redshift aiuta ad eliminare il rumore sulle stelle perchè le marca in maniera precisa, ma dai risultati
# ottenuti in quest'ultima iterazione risulta si possa arrivare ad una classificazione soddisfacente anche senza includerlo.

#############################################################
# ROC CURVES
#############################################################

# Previsioni di probabilità sui dati di test
tree_probabilities <- predict(tree_model_rpart, test, type = "prob")[, 2]
ctree_probabilities <- predict(ctree_model, test, type = "prob")[, 2]
rf_probabilities <- predict(rf, test, type = "prob")[, 2]
bag_probabilities <- predict(bag_model, test, type = "prob")[, 2]
gbm_predictions <- predict(gbm_model, test, n.trees = 200, type = "response") # Previsioni di probabilità per GBM 
# L'output di gbm_predictions sarà un array tridimensionale (n_samples, n_classes, n_trees)
# Media delle probabilità su tutti gli alberi:
gbm_probabilities <- apply(gbm_predictions, c(1, 2), mean)[, 2]

# Calcolo della curva ROC per ogni modello
tree_roc <- roc(test$class, tree_probabilities)
ctree_roc <- roc(test$class, ctree_probabilities)
rf_roc <- roc(test$class, rf_probabilities)
bag_roc <- roc(test$class, bag_probabilities)
gbm_roc <- roc(test$class, gbm_probabilities)

# Calcolare l'AUC per ogni modello
tree_auc <- auc(tree_roc)
ctree_auc <- auc(ctree_roc)
rf_auc <- auc(rf_roc)
bag_auc <- auc(bag_roc)
gbm_auc <- auc(gbm_roc)

# Creare un dataframe per la visualizzazione
roc_data <- data.frame(
  tpr = c(tree_roc$sensitivities, ctree_roc$sensitivities, rf_roc$sensitivities, bag_roc$sensitivities, gbm_roc$sensitivities),
  fpr = c(1 - tree_roc$specificities, 1 - ctree_roc$specificities, 1 - rf_roc$specificities, 1 - bag_roc$specificities, 1 - gbm_roc$specificities),
  model = factor(c(rep("Rpart", length(tree_roc$sensitivities)),
                   rep("Partykit", length(ctree_roc$sensitivities)),
                   rep("Random Forest", length(rf_roc$sensitivities)),
                   rep("Bagging", length(bag_roc$sensitivities)),
                   rep("Boosting", length(gbm_roc$sensitivities))))
)

# Visualizzare le curve ROC
ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
  geom_line() +
  geom_abline(linetype = "dashed") +
  labs(title = "ROC Curve Comparison",
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red", "purple", "orange"))

# Stampa dei valori AUC per confronto
print(paste("Tree (rpart) AUC:", tree_auc))
print(paste("Conditional Tree (partykit) AUC:", ctree_auc))
print(paste("Random Forest AUC:", rf_auc))
print(paste("Bagging AUC:", bag_auc))
print(paste("Boosting AUC:", gbm_auc))
