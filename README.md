## Datasets
To do such study, we have obtained the information from [Kaggle](https://www.kaggle.com/). The sets we will work with are the following: 
* [Dataset 1](https://www.kaggle.com/mousehead/songlyrics)
* [Dataset 2](https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics)

# Data Cleaning
Lyrics contain words that are not useful while finding the main feeling and meaning, so we have kept just the most meaningful , removing the so called stopwords. Moreover, we have created several functions relying on the library ```qdapDictionaries```.  First, "is.word", that returns whether the word introduced as a parameter is in the english-spanish language.
```R
is.word  <- function(x) x %in% GradyAugmented
```
Next, making use of the aforementioned function, the "get_existing_words" function is developed, which name says its functionallity. We can use such function to remove choirs and other words that may appear in the lyrics.
```R
get_existing_words <- function(x){
  lyric <- list()
  all_words <- unlist(stri_extract_all_words(x))
  for (w in all_words) {
    if (is.word(w)){
      lyric <- c(lyric, w)
    }
  }
  return(unlist(lyric))
}
```
# Visualizing the data
In order to understand the structure of the data in the dataframe obtained, here we show the words with the higher frequency in some of the most relevant Queen songs.

![alt text](/Memoria/Imagenes/AnotherOneBitesTheDust.png)

Other examples are shown below.

![alt tex](/Memoria/Imagenes/killerqueen.png)

![alt tex](/Memoria/Imagenes/loveofmylife.png$style=centerme)

Finally, after visualisation of the possible results of the functions applied to a given band we can build a graph from all the songs. As it is shown, the most common word used by the British band is "love".

![alt tex](/Memoria/Imagenes/queen_most_used_words.png)

Futhermore, if we study the most used words in a subset containing 400 songs, we obtain the result shown below.

![alt tex](/Memoria/Imagenes/Rplot_words.png)

As before, we can distinguish that the most used word by the Bitish band and other songs is **love**.

