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
# Words frecuency
In order to understand the structure of the data in the dataframe obtained, here we show the words with the higher frequency in some of the most relevant Queen songs.

![alt text](/Memoria/Imagenes/AnotherOneBitesTheDust.png)

Other examples are shown below.

![alt tex](/Memoria/Imagenes/killerqueen.png)

![alt tex](/Memoria/Imagenes/loveofmylife.png)

Finally, after visualisation of the possible results of the functions applied to a given band we can build a graph from all the songs. As it is shown, the most common word used by the British band is "love".

![alt tex](/Memoria/Imagenes/queen_most_used_words.png)

Futhermore, if we study the most used words in a subset containing 400 songs, we obtain the result shown below.

![alt tex](/Memoria/Imagenes/Rplot_words.png)

As before, we can distinguish that the most used word by the Bitish band and other songs is **love**.

# Sentiments frecuency

Based on the words extraction from the song lyrics, and using an existing dictionary which returns as key an existing relevant word and as value, the sentiment that is attached to it. 
```R
words_sentiments <- get_sentiments(lexicon = "nrc")
```
It may happen that a single word is attached to several sentiments, as it can be seen in the picture below.
![alt tex](/Memoria/Imagenes/words_sentiments.png)

Using the word extraction technique previously explained and this dictionary, we have explored which are the sentiments more used by the singer Adele in 12 of her songs. Before seeing the results, we established the hypothesis that the main sentiment in her work is *sadness* or *negativity*. The results can be seen in the image below.

![alt tex](/Memoria/Imagenes/Adele_sentiments.png)

The barplot shows that the most recurrent feeling is *positive*, which has resulted from the following reasons:
* The 12 songs in the dataset belonging to this Artist is not enough representative.
* Studying with the word scrapping technique, we can see that this are the most used words: 
![alt tex](/Memoria/Imagenes/Adele_words.png)
  *go* is not in the sentiments dictionary, but *love* does, and the sentiments attached to that word are *joy* and *positive*, so we can conclude that the usage of this dictionary does not allow us to have into account the context of the word.

