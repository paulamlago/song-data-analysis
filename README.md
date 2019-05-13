# Song data analysis: Contents
1. [Datasests](#datasets)
2. [Development metodology: KDD](#development-metodology-kdd)
    1. [Creating The Target Data Set](#creating-the-target-data-set)
        * [Data Cleaning And Transformation](#data-cleaning-and-transformation)
        * [Web Mining: Country Extraction](#web-mining-country-extraction)
    2. [Data Exploration](#data-exploration)
       * [Words Frecuency](#words-frecuency)
       * [Sentiments Frecuency](#sentiments-frecuency)
    3. [Data Mining Task: Clustering](#data-mining-task-clustering)
    4. [Data Mining Algorithm](#data-mining-algorithm)
    5. [Results](#results)

3. [Conclusions And Future Work](#conclusions-and-future-work)

# Datasets
To do such study, we have obtained the information from [Kaggle](https://www.kaggle.com/). The sets we will work with are the following: 
* [Dataset 1](https://www.kaggle.com/mousehead/songlyrics)
* [Dataset 2](https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics)

The first dataset has the following format. Artist, song name, web page containing extra information and finally the lyric.

![alt text](/Memoria/Imagenes/dataset1.png)

While the second one shows more information than the first dataset, such as the genre of the song or the release year, which we don't need.

![alt text](/Memoria/Imagenes/dataset2.png)

Both datasets needs a preprocesing to remove the extra information before starting to work.

# Development metodology: KDD

contar porqué, resumen

## Creating The Target Data Set
The final dataset is composed by a dataframe containing each artist's most frecuent sentiment and a second dataset containing the relation artist and country. The intersection of both datasets will be the final dataset.

### Data Cleaning And Transformation
Both input datasets needs to be reestructured, the first one has an extra column and the second dataset has two extra columns belonging to the genre of the song and the release year. Futhermore the order of the columns of both datasets needs to be set, so the second one has to be reordered to merge both dataframes. Finally, an extract of the final resulting dataframe will be the following, containing in the first column the artists, the song name and finally the lyrics. Some extracts of the final dataset, which contains 419887 entries are the following.

![alt text](/Memoria/Imagenes/pinkfloyd.png)

![alt text](/Memoria/Imagenes/amaral.png)

Lyrics contain words that are not useful or that include weird characters, some of them can be seen in the last image. Characters not found in the enlish dictionary doesn't show, instead a weird character appears. This words are not useful while finding the main feeling and meaning, so we have kept just the most meaningful , removing them and also the so called stopwords. Moreover, we have created several functions relying on the library ```qdapDictionaries```.  First, "is.word", that returns whether the word introduced as a parameter is in the english-spanish language.
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
As this funcion has to process every word on every lyric of 419887 songs, it takes too long to execute and we can't afford that computation capacity. Although it would be usefull, we will directly take the important words while extracting the sentiments present on each song. The text cleaning made at this point consists on erasing the storpwords such as conjunctions or articles, but also the song's typical sounds, as it can be seen in the next piece of code.

```R
ASCL[,3] <- removeWords(as.character(ASCL[,3]), words = c(stopwords("english"), "oh", "ah", "eh", "uh", "ma"))  #stopwords estan en minuscula
ASCL[,3] <- stripWhitespace(ASCL[,3])
ASCL[,3] <- removePunctuation(ASCL[,3])
```
### Sentiment Extraction
Based on the words extraction from the song lyrics, and using an existing dictionary which returns as key an existing relevant word and as value, the sentiment that is attached to it. 
```R
words_sentiments <- get_sentiments(lexicon = "nrc")
```
It may happen that a single word is attached to several sentiments, as it can be seen in the picture below.

![alt tex](/Memoria/Imagenes/words_sentiments.png)

Then, we have traversed in a loop all the different artists and we have concatenated their lyrics, this way, by getting the intersection of each word and the dicctionary above, we get the sentiments that those words evoque. This way, ge can get a list of all the sentiments that an artist share with each word int he lyrics.

![alt tex](/Memoria/Imagenes/Adelesentiments.png)

After getting this intermediate dataframe, we can extract the frecuency in which each sentiment shows and get the maximum one, to generalice and obtain a general view of the sentiments that each artist try to share based on the R sentiments dicctionary.

![alt tex](/Memoria/Imagenes/artistsentiment.png)

### Procedence Country
The second dataframe needed to continue with the study is the one that establishes the relation between the artists or bands with its procedence country. The metodology followed to obtain that data is explained in the section [Web Mining: Country Extraction](#web-mining-country-extraction). The final dataset is shown in the following picture.

![alt text](/Memoria/Imagenes/artistcountry.png)

The fist column belongs to the artist, the second to the number of songs and the last column is the procedence country.

### Example Of The Final Dataset
After extracting each artist's primary sentiment and the procedence country of every artist, we can elaborate a table in which the frequency of feelings will be shown.

![alt text](/Memoria/Imagenes/sentimenttablebycountry.png)

## Data Exploration
### Words Frecuency
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

### Sentiments Frecuency



Using the word extraction technique previously explained and this dictionary, we have explored which are the sentiments more used by the singer Adele in 12 of her songs. Before seeing the results, we established the hypothesis that the main sentiment in her work is *sadness* or *negativity*. The results can be seen in the images below.

![alt tex](/Memoria/Imagenes/Adele_sentiments.png)

![alt tex](/Memoria/Imagenes/Adele_wordcloud.png)

The barplot shows that the most recurrent feeling is *positive*, which has resulted from the following reasons:
* The 12 songs in the dataset belonging to this Artist is not enough representative.
* Studying with the word scrapping technique, we can see that this are the most used words: 
![alt tex](/Memoria/Imagenes/Adele_words.png)

  *go* is not in the sentiments dictionary, but *love* does, and the sentiments attached to that word are *joy* and *positive*, so we can conclude that the usage of this dictionary does not allow us to have into account the context of the word.

As we previously studied Queen's most used words, we have also extracted which are the most present sentiments.

![alt tex](/Memoria/Imagenes/Queen_wordcloud.png)

To conclude, we have extracted the most frecuent sentiment for each artist or band in the dataset and we have shown in the following graph which are the feelings more frecuently used.

![alt tex](/Memoria/Imagenes/most_frecuent_sentiments.png)

## Data Mining Task: Clustering

## Data Mining Algorithm
## Web Mining: Country Extraction
In order to establish a relation between the artist's sentiment and the country's we need to obtain the precedence country of each artist in the dataset. Using **Wikipedia**, we can obtain that information, taking into account that each artist's url can have different formats, we have tried with every possibility, traversing each artist in the dataset.

```R
pwebs <- c(paste("https://es.wikipedia.org/wiki/", artists[i, 1], sep=""),
             paste("https://es.wikipedia.org/wiki/", artists[i, 1], "_(banda)", sep=""),
             paste("https://es.wikipedia.org/wiki/", artists[i, 1], "_(cantante)", sep=""),
             paste("https://en.wikipedia.org/wiki/", artists[i, 1], sep=""),
             paste("https://en.wikipedia.org/wiki/", artists[i, 1], "_(singer)", sep=""),
             paste("https://en.wikipedia.org/wiki/", artists[i, 1], "_(band)", sep=""))
```
Those are all the formats that we have found in which an artist or band web page is shown. After that, by using the function ```  exists <- sapply(pwebs, url_exists)``` we obtain which of the following links exists, and just by taking the first one that exists we can access the table that contains the personal information.
## Results

# Conclusions And Future Work
Qué hemos conseguido, qué no, y porqué. Cosas que reconocemos que nos ha faltado y puntos fuertes en los que hemos trabajado mucho
También contar qué podemos mejorar en el futuro
