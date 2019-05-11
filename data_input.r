if (!require(stringr)) {install.packages("stringr")}
if (!require(rvest)) {install.packages("rvest")}
if (!require(tm)) {install.packages("tm")} # Si no funciona la instalaciÃƒÂ³n, probar instalando antes el paquete XML
if (!require(SnowballC)) {install.packages("SnowballC")}
if (!require(rlist)) {install.packages("rlist")}
#install.packages("hash")
if (!require(ngram)) {install.packages("ngram")}
if (!require(qdapDictionaries)) {install.packages("qdapDictionaries")}
if (!require(RCurl)) {install.packages("RCurl")}
if (!require(countrycode)) {install.packages("countrycode")}
if (!require(rvest)) {install.packages("rvest")}
if (!require(tidytext)) {install.packages("tidytext")}
if (!require(dplyr)) {install.packages("dplyr")}
if (!require(dplyr)) {install.packages("maps")}
if (!require(dplyr)) {install.packages("plyr")}
#################LIBRERIAS VISUALIZACION###################
if (!require(wordcloud)) {install.packages("wordcloud")}
if (!require(RColorBrewer)) {install.packages("RColorBrewer")}
##########################################################
library(maps)
library(plyr)
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

split_words <- function(f_list){
  return(f_list <- stri_extract_all_words(f_list)[[1]])
}

extract_country_es <- function(table, countries, world.cities){
  k <- FALSE
  for (i in 1:length(table)) {
    #encontramos la tabla que contiene la info requerida (si existe)
    if("Origen" %in% (table[i]%>%html_nodes("th")%>%html_text())|"Nacionalidad" %in% (table[i]%>%html_nodes("th")%>%html_text())|"Nacimiento" %in% (table[i]%>%html_nodes("th")%>%html_text())){      
      table <- table[i]
      k <- TRUE
      break
    }
  }
  if(k){
    #filtramos por nodos para hacer la busqueda mas eficiente
    table <- table %>% html_nodes("td") %>% html_nodes("a") %>% html_text()
    table <- table[1:10]
    if("Estadounidense" %in% table) return("Estados Unidos")
    if("Inglaterra" %in% table) return("Reino Unido")
    #buscamos el pais en la primera lista de paises
    country <- get_country1(countries, table)
    #si no, en la segunda
    if(is.null(country)){
      #para ello separamos las palabras y comprobamos una a una por ciudad
      table <- paste(table, collapse = " ")
      table <- split_words(table)
      n <- get_country2(world.cities, table)
      if(n > 0){
        #si n>0 hay al menos un pais con esa ciudad
        country <- world.cities$country.etc[n]
      }
    }
    return(country)
  }
  return(NULL)
}

extract_country_en <- function(table, countries, world.cities){
  k <- FALSE
  for (i in 1:length(table)) {
    #encontramos la tabla que contiene la info requerida (si existe)
    if("Origin" %in% (table[i]%>%html_nodes("th")%>%html_text())|"Born" %in% (table[i]%>%html_nodes("th")%>%html_text())){
      table <- table[i]
      k <- TRUE
      break
    }
  }
  if(k){
    #filtramos por nodos para hacer la busqueda mas eficiente
    table <- table %>% html_nodes("td") %>% html_nodes("a") %>% html_text()
    table <- table[1:10]
    #buscamos el pais en la primera lista de paises
    country <- get_country1(countries, table)
    #si no, en la segunda
    if(is.null(country)){
      #para ello separamos las palabras y comprobamos una a una por ciudad
      table <- paste(table, collapse = " ")
      table <- split_words(table)
      n <- get_country2(world.cities, table)     
      if(n > 0){
        #si n>0 hay al menos un pais con esa ciudad
        country <- world.cities$country.etc[n]
      }
    }
    return(country)
  }
  return(NULL)
}

get_country1 <- function(country_list, origin){
  #busqueda en las listas de paises de countrycode
  for (k in 1:length(origin)) {
    if(origin[k] %in% country_list){
      return(origin[k])
    }
  }
}

get_country2 <- function(cities, origin){
  #busqueda en la base de datos de world.cities
  for (k in 1:length(origin)) {
    n <- which(origin[k] == cities$name)
    if(length(n) >= 1) {return(n[1])}
  }
  return(0)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

url_exists <- function(x) url.exists(as.character(x))

##############################################################################################################################
#################################################  EXTRACTION AND DATA STRUCTURE  ########################################################
#primer dataset: songdata.csv
ASCL <- read.csv(paste(getwd(), "/Data/songlyrics/songdata.csv", sep = ""), header = TRUE, sep = ",", nrows = 5000, colClasses = c(NA, NA, "NULL", NA))
#la tercera columna son las letras
names(ASCL)[3] <- "lyrics"
#segundo dataset: lyrics.csv
aux <- read.csv(paste(getwd(), "/Data/songlyrics/lyrics.csv", sep = ""), header = TRUE, sep = ",", nrows = 5000, colClasses = c("NULL", NA, "NULL", NA, "NULL", NA))
aux <- aux[,c(2, 1, 3)]#mismo orden de columas que aux
ASCL<- rbind(ASCL, aux)#concatenar los data frames
rm(aux)
ASCL <- data.frame(ASCL)
# Convertimos a minúsculas cada palabra, no solo de la letra de las canciones, sino del título de la canción
#Problema -> Al hacer apply devuelve una lista en vez de un data.frame, así que volvemos a convertirlo a data.frame
# la función t() devuelve la matriz transpuesta, ya que por alguna razón apply devuelve las columnas como filas y al revés

#for(i in 1:length(ASCL))
#  ASCL[i] <- lapply(ASCL[i],toupper) #pasar el dataframe a mayusc

################################################################################################################################
############################################  DATA CLEANING  ###################################################################
#DATAFRAME CONTAINING ALL INFO 
ASCL <- setNames(ASCL <- data.frame(ASCL[[1]],apply(ASCL[2:3], 2, tolower)), c("artist","song","lyrics")) #no transformamos la columna de artistas
ASCL[,3] <- removeWords(as.character(ASCL[,3]), words = c(stopwords("english"), "oh", "ah", "eh", "uh", "ma"))  #stopwords estan en minuscula
ASCL[,3] <- stripWhitespace(ASCL[,3])
ASCL[,3] <- removePunctuation(ASCL[,3])
ASCL[,3] <- removeNumbers(ASCL[,3]) # Por si acaso

#ARTIST INFO CLEANING 
artists <- cbind(as.data.frame(table(ASCL$artist), stringsAsFactors = FALSE), NA) #aquí van los artistas con su frecuencia en el dataframe
#separamos las palabras que componen el nombre del artista por espacios
artists[,1] <- str_replace_all(artists[,1],"-"," ")
artists[,1] <- str_replace_all(artists[,1],"_"," ")
#Pasamos a mayus la primera letra de cada palabra y unimos con '_' para busqueda en la web
for (i in 1:length(artists$Var1)) {
  artists[i, 1] <- paste(unlist(firstup(split_words(artists[i, 1]))), collapse = "_")  
}
#pasamos los articulos que mas aparecen a minuscula
artists[,1] <- str_replace_all(artists[,1],"Of","of") #queda pulir los artículos de los artistas
artists[,1] <- str_replace_all(artists[,1],"This","of") #queda pulir los artículos de los artistas
artists[,1] <- str_replace_all(artists[,1],"With","of") #queda pulir los artículos de los artistas
artists[,1] <- str_replace_all(artists[,1]," ","_") #en las url los espacios se sustituyen por '_'
#cambaimos el nombre de las columnas
colnames(artists) <- c("Artist", "Freq","Country")

##########################################################################################################################
#################################### DATA INTERPRETATION ##############################################

#GET ALL WORDS IN DATASET 
existing_words_in_all_set <- get_existing_words(ASCL[,3]) #comprueba palabras en inglés y castellano siempre en minusuclas
words.freq <- table(existing_words_in_all_set)#extraemos la frecuencia con la que aparece cada palabra
words_data <- cbind.data.frame(names(words.freq),as.integer(words.freq)) #unimos palabras con frecuencias y combinamos
names(words_data) <- c("word", "repetitions")
words_data <- words_data[order(words_data$repetitions, decreasing = TRUE)[1:10], ] #Cogemos las 10 palabras con más apariciones

#Get words from a certain band
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

#EXTRACCIÓN DEL SENTIMIENTO DE CADA CANCIÓN
words_sentiments <- get_sentiments(lexicon = "nrc") #Data frame palabra,sentimiento
#En primer lugar, tenemos que dividir el texto de las canciones en palabras
Adele_songs <- data.frame(ASCL[134:145,])[3] #Cogemos 3 porque no nos interesa nada más que la letra
Adele_tokens <- strsplit(Adele_songs[, 1], " ")
Adele_all_tokens <- unlist(Adele_tokens)
Adele_all_tokens <- stripWhitespace(Adele_all_tokens)
Adele_sentiments <- list()
Adele_word_sentiment <- data.frame()
for (token in Adele_all_tokens){
  if (any(words_sentiments$word == token)){
    Adele_sentiments <- words_sentiments[which(words_sentiments$word == token), 2]
    Adele_word_sentiment <- rbind(df, data.frame(token, Adele_sentiments))
  }
}

Adele_sentiments_list <- unlist(Adele_word_sentiment[,2])
sentiments.freq <- table(Adele_sentiments_list)
Adele_sentiments <- cbind.data.frame(names(sentiments.freq), as.integer(sentiments.freq))


#################################################################################################################
################### OBTENCIÓN DEL PAIS DE CADA AUTOR #######################
#Todos los países en castellano e ingles, para comprobar con las string que obtengamos
existing_countries_es <- countrycode::codelist$cldr.name.es 
existing_countries_en <- countrycode::codelist$cldr.name.en 
#dataset con ciudades vinculadas a paises
data(world.cities)
#La intención es recorrer los artistas, crear la url de Wikipedia y encontrar la tabla que contenga la info que necesitamos

#ejecutamos diferentes loops para buscar el país de procedencia del artista teniendo en cuenta que pueden faltar detalles como
# poner después del artista (banda) para que sea reconocible por wikipedia
for(i in 1:length(artists$Artist)){
  pwebs <- c(paste("https://es.wikipedia.org/wiki/", artists[i, 1], sep=""),
             paste("https://es.wikipedia.org/wiki/", artists[i, 1], "_(banda)", sep=""),
             paste("https://es.wikipedia.org/wiki/", artists[i, 1], "_(cantante)", sep=""),
             paste("https://en.wikipedia.org/wiki/", artists[i, 1], sep=""),
             paste("https://en.wikipedia.org/wiki/", artists[i, 1], "_(singer)", sep=""),
             paste("https://en.wikipedia.org/wiki/", artists[i, 1], "_(band)", sep="")) #Todas las posibilidades
  exists <- sapply(pwebs, url_exists)
  if (any(exists) == TRUE){
    index <- which(exists == TRUE)[1] #En el caso de que existan varias, cogemos la primera
    print(i)
    page <- read_html(pwebs[index])
    a <- page %>% html_nodes("table")
    country <- extract_country_es(a, existing_countries_es, world.cities)
    if(is.null(country)){
      country <- extract_country_en(a, existing_countries_en, world.cities)
      if(is.null(country)){
        #Borramos la fila del dataframe
        artists <- artists[-c(i), ]
      }
    } else { artists[i,3] <- country }
    
  } else {
    print(paste(i, " doesn't exists"))
    artists <- artists[c(-i), ] #Si no está en ninguna de las pwebs -> borramos la fila
  }
}

# NA_Artists <- which(is.na(artists$Country)) #devuelve los índices de los paises que no tienen información
# for(i in NA_Artists){
#   pweb <- paste("https://es.wikipedia.org/wiki/", artists[i, 1], "_(banda)", sep="")
#   if (url.exists(pweb)){
#     page <- read_html(pweb)
#     a <- page %>% html_nodes("table")
#     country <- extract_country_es(a, existing_countries_es, world.cities)
#     if(is.null(country)){
#       artists[i,3] <- NA
#     }
#     else{
#       artists[i,3] <- country
#     }
#   }
#   else{
#     artists[i,3] <- NA
#   }
# }

# for(i in 1:length(artists$Artist)){
#   if(is.na(artists$Country[i])){
#     pweb <- paste("https://es.wikipedia.org/wiki/", artists[i, 1], "_(cantante)", sep="")
#     if (url.exists(pweb)){  
#       page <- read_html(pweb)
#       a <- page %>% html_nodes("table")
#       country <- extract_country_es(a, existing_countries_es, world.cities)
#       if(is.null(country)){
#         artists[i,3] <- NA
#       }
#       else{
#         artists[i,3] <- country
#       }
#     }
#     else{
#       artists[i,3] <- NA
#     }
#   }
# }
# 
# for(i in 1:length(artists$Artist)){
#   if(is.na(artists$Country[i])){
#     pweb <- paste("https://en.wikipedia.org/wiki/", artists[i, 1], sep="")
#     if (url.exists(pweb)){  
#       page <- read_html(pweb)
#       a <- page %>% html_nodes("table")
#       country <- extract_country_en(a, existing_countries_en, world.cities)
#       if(is.null(country)){
#         artists[i,3] <- NA
#       }
#       else{
#         artists[i,3] <- country
#       }
#     }
#     else{
#       artists[i,3] <- NA
#     }
#   }
# }
# 
# for(i in 1:length(artists$Artist)){
#   if(is.na(artists$Country[i])){
#     pweb <- paste("https://en.wikipedia.org/wiki/", artists[i, 1], "_(band)", sep="")
#     if (url.exists(pweb)){  
#       page <- read_html(pweb)
#       a <- page %>% html_nodes("table")
#       country <- extract_country_en(a, existing_countries_en, world.cities)
#       if(is.null(country)){
#         artists[i,3] <- NA
#       }
#       else{
#         artists[i,3] <- country
#       }
#     }
#     else{
#       artists[i,3] <- NA
#     }
#   }
# }
# 
# for(i in 1:length(artists$Artist)){
#   if(is.na(artists$Country[i])){
#     pweb <- paste("https://en.wikipedia.org/wiki/", artists[i, 1], "_(singer)", sep="")
#     if (url.exists(pweb)){  
#       page <- read_html(pweb)
#       a <- page %>% html_nodes("table")
#       country <- extract_country_en(a, existing_countries_en, world.cities)
#       if(is.null(country)){
#         artists[i,3] <- NA
#       }
#       else{
#         artists[i,3] <- country
#       }
#     }
#     else{
#       artists[i,3] <- NA
#     }
#   }
# }

#traducimos al castellano los paises obtenidos mediante el algoritmo
for(i in 1:length(artists$Country)){
  if(artists$Country[i]%in%existing_countries_en){
    artists$Country[i] <- countrycode(artists$Country[i], "country.name", "cldr.name.es")
  }
  else if(artists$Country[i]=="UK"|artists$Country[i]=="USA"){
    artists$Country[i] <- countrycode(artists$Country[i], "country.name", "cldr.name.es")
  }
}
#buscamos que los artistas con mayor número de canciones tengan país de procedencia ya que serán más relevantes


rm(country)

########################################################################################################################
##############################VISUALIZACION DE DATOS######################
plot(auths_count) #visualizacion de los datos antes de agrupar
wordcloud(words_data[,1], freq = words_data[,2],min.freq = 1, random.order = FALSE,color= brewer.pal(8, "Dark2"), max.words = 500)

#Visualización 10 palabras más utilizadas
pal <- colorRampPalette(colors = c("blue", "lightblue"))(length(words_data[[1]]))
barplot(words_data$repetitions, 
        names.arg = words_data$word,
        col = pal,
        xlab = "Words",
        ylab = "Repetitions")

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