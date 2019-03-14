install.packages("stringr")
install.packages("rvest")
install.packages("tm") # Si no funciona la instalación, probar instalando antes el paquete XML
install.packages("SnowballC")
install.packages("rlist")
install.packages("hash")
library(tm)
library(hash) #para crear y operar con estructuras hash
library(stringr)
library(stringi)

#primer dataset: songdata.csv
ASCL <- read.csv(paste(getwd(), "/Data/songlyrics/songdata.csv", sep = ""), header = TRUE, sep = ",", nrows = 5, colClasses = c(NA, NA, "NULL", NA))
#la tercera columna son las letras
names(ASCL)[3] <- "lyrics"
#segundo dataset: lyrics.csv
aux <- read.csv(paste(getwd(), "/Data/songlyrics/lyrics.csv", sep = ""), header = TRUE, sep = ",", nrows = 5, colClasses = c("NULL", NA, "NULL", NA, "NULL", NA))
#mismo orden de columas que ASCL1
aux <- aux[,c(2, 1, 3)]
#concatenar los data frames
ASCL <- rbind(ASCL, aux)

# Convertimos a minúsculas cada palabra, no solo de la letra de las canciones, sino del nombre del grupo y canción
#Problema -> Al hacer apply devuelve una lista en vez de un data.frame, así que volvemos a convertirlo a data.frame
# la función t() devuelve la matriz transpuesta, ya que por alguna razón apply devuelve las columnas como filas y al revés
ASCL <- data.frame(t(apply(ASCL, 1, tolower))) #stopwords estan en minuscula
ASCL[,3] <- removeWords(as.character(ASCL[,3]), words = stopwords("english")) 
ASCL[,3] <- stripWhitespace(ASCL[,3])
ASCL[,3] <- removePunctuation(ASCL[,3])
ASCL[,3] <- removeNumbers(ASCL[,3]) # Por si acaso
for(i in 1:length(ASCL))
  ASCL[i] <- lapply(ASCL[i], toupper) #pasar el dataframe a mayusc

#############################################################################
#dividir la string que contiene la letra en palabras
words = unlist(stri_extract_all_words(ASCL[[3]][1]))
#esta es la idea aunque no funciona:
for(i in 1:length(ASCL[[3]])){
  words <- unlist(stri_extract_all_words(ASCL[[3]][i]))
  lyrics[i] <- words
}
#obtenemos las columnas del dataframe por separado para poder trabajar y crear el hash
auths <- levels(ASCL[[1]])
sNames <- ASCL[[2]]
lyrics <- rep(list("character"), length(ASCL[[3]]))
#en este ejemplo creamos un hash con clave el nombre de autor
#y el valor otro hash con clave cancion y valor letra
h <- hash(ASCL[[2]], ASCL[[3]])
#un hash como valor por cada autor como clave
o <- hash(auths, c(h, h, h, h, h))

############################################################################
#VISUALIZACION DE DATOS
auths_count <- as.data.frame(table(ASCL$artist)) #contabilidad de el numero de veces que aparece cada autor en el dataframe
plot(auths_count)  #visualizacion de los datos antes de agrupar