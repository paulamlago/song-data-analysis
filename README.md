## Conjunto de datos
Para la realización del estudio, obtendremos la información de [Kaggle](https://www.kaggle.com/). Los conjuntos a utilizar serán: 
* [Dataset 1](https://www.kaggle.com/mousehead/songlyrics)
* [Dataset 2](https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics)

# Limpieza
Puesto que las letras de las canciones contienen palabras que no son relevantes a la hora de extraer el significado y sentimiento general, hemos extraido las palabras "útiles", eliminando las llamadas "stopwords" del inglés. Además hemos desarrollado varias funciones, con el apoyo de la librería ```qdapDictionaries```. En primer lugar, "is.word" se encargará de devolver si la palabra que le pasamos como parámetro está en el diccionario de castellano o de ingés o no.
```R
is.word  <- function(x) x %in% GradyAugmented
```
En segundo lugar, haciendo uso de la función anterior, hemos desarrollado get_existing_words, que dada la lestra de una canción, solo devuelve aquellas palabras que existen en el diccionario. Esto nos es útil para eliminar coros y otras palabras que no nos son útiles que pueden aparecer en la letra.
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
# Visualización
Con el fin de entender cómo están estructurados los datos en el dataset obtenido, hemos visualizado las palabras más utilizadas en las canciones más conocidas de Queen.

![alt text](/Memoria/Imagenes/AnotherOneBitesTheDust.png)

Cualquier persona que haya escuchado esta canción puede corroborar que efectivamente, esas son las palabras más utilizadas. Otros ejemplos pueden verse en las siguientes imágenes.

![alt tex](/Memoria/Imagenes/killerqueen.png)

![alt tex](/Memoria/Imagenes/loveofmylife.png)

Finalmente, tras visualizar los datos que podemos extraer de las canciones de un cierto grupo, elaboramos una gráfica obteniendo información de todas sus canciones. Como se aprecia, la palabra que más se repite en las canciones del grupo británico es amor.

![alt tex](/Memoria/Imagenes/queen_most_used_words.png)
