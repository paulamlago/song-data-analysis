# Analisis de las letras de las canciones por país de procedencia
Proyecto para la asignatura Minería de datos y el paradigma del Big Data desarrollado en el curso 2018/2019. El objetivo es estudiar las palabras presentes en las letras de las canciones de cada país para conseguir extraer un sentimiento general presente en dicho país.

## Conjunto de datos
Para la realización del estudio, obtendremos la información de https://www.kaggle.com/. Los conjuntos a utilizar serán: 
* [Dataset 1](https://www.kaggle.com/mousehead/songlyrics)
* [Dataset 2](https://www.kaggle.com/gyani95/380000-lyrics-from-metrolyrics)

## Metodología a seguir
Para encontrar el país de procedencia de cada autor(a), utilizaremos técnicas de minería web para obtener dicha información a través de Wikipedia u otras fuentes. Una vez obtenida la información (país, autor, letras de sus canciones), limpiaremos los datos escogiendo sólo las palabras más relevantes, siendo éstas los verbos, sustantivos y adjetivos, eliminando artículos, conjunciones.
Una vez obtenido el conjunto de datos sobre el cual trabajar, extraemos la palabra más utilizada primero por artista y luego por país. Plantearemos y trabajaremos sobre la siguiente hipótesis: según la región, la temática de las canciones está orientada hacia un sentimiento predominante. Podremos comprobarlo si las palabras más utilizadas son del campo semántico.
