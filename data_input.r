install.packages("stringr")
install.packages("rvest")
install.packages("tm") # Si no funciona la instalaciÃ³n, probar instalando antes el paquete XML
install.packages("SnowballC")
install.packages("rlist")
#install.packages("hash")
install.packages("ngram")
install.packages("qdapDictionaries")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("RCurl")
install.packages("countrycode")
install.packages("rvest")
library(rvest)
library(countrycode)
library(RColorBrewer)
library(wordcloud)
library(tm)
#library(hash) #para crear y operar con estructuras hash
library(stringr)
library(stringi)
library(ngram)
library(qdapDictionaries)
library(RCurl)
###############################################################################################################################
##################################################DECLARACIÓN DE FUNCIONES#####################################################
#Comprobamos si la palabra está en el diccionario
is.word  <- function(x) x %in% GradyAugmented

get_existing_words <- function(x){ #Tarda mucho :(
  lyric <- list()
  all_words <- unlist(stri_extract_all_words(x))
  for (w in all_words) {
    if (is.word(w)){
      lyric <- c(lyric, w)
    }
  }
  return(unlist(lyric))
}

get_country <- function(country_list, origin){
  for (country in country_list){
    if (grepl(country, origin)){
      return(country)
    }
  }
}
##############################################################################################################################

#primer dataset: songdata.csv
ASCL <- read.csv(paste(getwd(), "/songdata.csv", sep = ""), header = TRUE, sep = ",", nrows = 200, colClasses = c(NA, NA, "NULL", NA))
#la tercera columna son las letras
names(ASCL)[3] <- "lyrics"
#segundo dataset: lyrics.csv
aux <- read.csv(paste(getwd(), "/lyrics.csv", sep = ""), header = TRUE, sep = ",", nrows = 200, colClasses = c("NULL", NA, "NULL", NA, "NULL", NA))
#mismo orden de columas que ASCL1
aux <- aux[,c(2, 1, 3)]
#concatenar los data frames
ASCL<- rbind(ASCL, aux)
rm(aux)
# Convertimos a minúsculas cada palabra, no solo de la letra de las canciones, sino del nombre del grupo y canción
#Problema -> Al hacer apply devuelve una lista en vez de un data.frame, así que volvemos a convertirlo a data.frame
# la función t() devuelve la matriz transpuesta, ya que por alguna razón apply devuelve las columnas como filas y al revés
ASCL <- data.frame(ASCL)
ASCL <- setNames(ASCL <- data.frame(ASCL[[1]],apply(ASCL[2:3], 2, tolower)), c("artist","song","lyrics")) #no transformamos la columna de artistas
ASCL[,3] <- removeWords(as.character(ASCL[,3]), words = c(stopwords("english"), "oh", "ah", "eh", "uh", "ma"))  #stopwords estan en minuscula
ASCL[,3] <- stripWhitespace(ASCL[,3])
ASCL[,3] <- removePunctuation(ASCL[,3])
ASCL[,3] <- removeNumbers(ASCL[,3]) # Por si acaso
#for(i in 1:length(ASCL))
#  ASCL[i] <- lapply(ASCL[i],toupper) #pasar el dataframe a mayusc
#############################################################################
### Get all different words in the whole dataset ###
existing_words_in_all_set <- get_existing_words(ASCL[,3]) #comprueba palabras en inglés y castellano siempre en minusuclas
words.freq <- table(existing_words_in_all_set)#extraemos la frecuencia con la que aparece cada palabra
words_data <- cbind.data.frame(names(words.freq),as.integer(words.freq)) #unimos palabras con frecuencias y combinamos
names(words_data) <- c("word", "repetitions")
words_data <- words_data[order(words_data$repetitions, decreasing = TRUE)[1:10], ] #Cogemos las 10 palabras con más apariciones
### Get words from a certain band
queen_songs <- ASCL[ASCL[1] == "Queen",,]
queen_songs <- data.frame(queen_songs[2],queen_songs[3]) #We don't need the band name
#Choose certain songs to explore
songs_to_select <- which(queen_songs$song %in% c("love of my life", "somebody to love", "bohemian rhapsody", "killer queen", "the show must go on"))
queen_songs_selected <- queen_songs[songs_to_select,]
pal <- colorRampPalette(colors = c("blue", "lightblue"))(length(words_data[[1]]))
for(s in 1:nrow(queen_songs_selected)){ #Veremos las palabras más utilizadas en cada canción
  song <- queen_songs_selected[s,,]
  print(song$song)
  song_lyric <- get_existing_words(song$lyrics)
  words.freq <- table(song_lyric)
  words_data <- cbind.data.frame(names(words.freq),as.integer(words.freq))
  names(words_data) <- c("word", "repetitions")
  words_data <- words_data[order(words_data$repetitions, decreasing = TRUE)[1:10], ] #Cogemos las 10 palabras con más apariciones
  fname <- paste("/home/paulamlago/Documents/Uni/MIN/Analisis-de-letras-de-canciones/", str_replace_all(song$song, " ",""), ".png", sep="")
  png(filename = fname)
  barplot(words_data$repetitions, 
          names.arg = words_data$word,
          col = pal,
          xlab = "Words",
          ylab = "Repetitions",
          main =song$song,
          las = 2)
  dev.off()
}
#Visualize the most used words from that band
queen_most_used_words <- get_existing_words(queen_songs$lyrics)
words.freq <- table(queen_most_used_words)
words_data <- cbind.data.frame(names(words.freq),as.integer(words.freq))
names(words_data) <- c("word", "repetitions")
pal <- colorRampPalette(colors = c("orange", "white"))(length(words_data[[1]]))
words_data <- words_data[order(words_data$repetitions, decreasing = TRUE)[1:15], ] #Cogemos las 15 palabras con más apariciones
fname <- paste("/home/paulamlago/Documents/Uni/MIN/Analisis-de-letras-de-canciones/queen_most_used_words", ".png", sep="")
png(filename = fname)
barplot(words_data$repetitions, 
        names.arg = words_data$word,
        col = pal,
        xlab = "Words",
        ylab = "Repetitions",
        las = 2)
dev.off()

############################################################################
################### OBTENCIÓN DEL PAIS DE CADA AUTOR #######################
existing_countries <- countrycode::codelist$cldr.name.es #Todos los países en castellano, para matchearlo con las string que obtengamos
#Extraer la lista de países, ahora están en una string

#La intención es recorrer los artistas, crear la url de Wikipedia, que no funciona
#porque tiene que ser la primera letra mayúscula, aunque en el caso de abba tiene que
#ser ABBA, por lo que en la lectura no debermos pasar los nombres de los grupos a minúscula!!
#Además, en Wikipedia siempre está la etiqueta Origen, donde viene el país en último lugar
#El código funciona para el ejemplo de Queen, tenemos que conseguir que funcione el bucle
#y crear un nuevo data frame artista - país
artists <- as.data.frame(table(ASCL$artist)) #aquí van los artistas con su frecuencia en el dataframe
artists[,1] <- str_replace_all(artists[,1],"Of","of") #queda pulir los artículos de los artistas
artists[,1] <- str_replace_all(artists[,1]," ","_") #en las url los espacios se sustituyen por '_'
artist_country <- data.frame(matrix(ncol=2, nrow=length(artists[[1]]))) #introducimos aquí el artista y el país de procedencia
colnames(artist_country) <- c("Artist", "Country")

for(i in 1:length(artists[[1]])){
  pweb <- paste("https://es.wikipedia.org/wiki/", artists[i, 1], sep="")
  if (url.exists(pweb)){
    page <- read_html(pweb)
    htmltable <- html_table(page)[1]
    data <- as.data.frame(htmltable)[,-2]
    origin_info <- data[which(grepl("Origen", data[[1]])),2]
    country <- get_country(existing_countries, origin_info)
    artist_country[i, ] <- c(artists[i, 1], country)
  } else{
    print("No information for ", artists[i, 1])
  }
}
rm(data)
rm(htmltable)
rm(country)
rm(origin_info)
artists <- cbind(artists, artist_country)   #meter el pais y el artista en un data frame

############################################################################
#VISUALIZACION DE DATOS
plot(auths_count) #visualizacion de los datos antes de agrupar
wordcloud(words_data[,1], freq = words_data[,2],min.freq = 1, random.order = FALSE,color= brewer.pal(8, "Dark2"), max.words = 500)
#Visualización 10 palabras más utilizadas
pal <- colorRampPalette(colors = c("blue", "lightblue"))(length(words_data[[1]]))
barplot(words_data$repetitions, 
        names.arg = words_data$word,
        col = pal,
        xlab = "Words",
        ylab = "Repetitions")