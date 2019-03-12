install.packages("stringr")
install.packages("rvest")
install.packages("tm") # Si no funciona la instalación, probar instalando antes el paquete XML
install.packages("SnowballC")
install.packages("rlist")
install.packages("hash")
library(tm)
library(hash) #para crear y operar con estructuras hash
library(stringr)
ASCL <- read.csv(paste(getwd(), "/Data/songlyrics/songdata.csv", sep = ""), header = TRUE, sep = ",", nrows = 200, colClasses = c(NA, NA, "NULL", NA))
# Convertimos a minúsculas cada palabra, no solo de la letra de las canciones, sino del nombre del grupo y canción
#Problema -> Al hacer apply devuelve una lista en vez de un data.frame, así que volvemos a convertirlo a data.frame
# la función t() devuelve la matriz transpuesta, ya que por alguna razón apply devuelve las columnas como filas y al revés
ASCL <- data.frame(t(apply(ASCL, 1, toupper)))
ASCL[[3]] <- str_replace_all(ASCL[[3]], "[\r\n]" , "") #eliminamos todos los newline \n
ASCL[,3] <- removeWords(as.character(ASCL[,3]), words = stopwords("english")) 
ASCL[,3] <- removePunctuation(ASCL[,3])
ASCL[,3] <- removeNumbers(ASCL[,3]) # Por si acaso
ASCL[,3] <- stripWhitespace(ASCL[,3])

