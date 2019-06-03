#### Instalar y Cargar Paquetes a ser utilizados en mineria de texto ####
# Instalar Paquetes
install.packages('NLP', dependencies=TRUE, repos='http://cran.rstudio.com/')  # Es dependencia del paquete TM
install.packages('tm', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('RColorBrewer', dependencies=TRUE, repos='http://cran.rstudio.com/') # Es dependencia del paquete wordcloud 
install.packages('wordcloud', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('readr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('cluster', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('SnowballC', dependencies=TRUE, repos='http://cran.rstudio.com/')

# Cargar Paquetes
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)
library(SnowballC)


#### Cargar el archivo a realizarle Text Minig ####
Archivo_OAM <- read_lines(file.choose())


#### Limpiar el archivo de observaciones (Limpieza de Texto) ####

# Eliminar los espacios en blanco de exceso 
Observaciones <- stripWhitespace(Archivo_OAM)

# Convertir todo a minusculas
Observaciones <- tolower(Observaciones)

# Eliminar palabras vacias, aquellas con poco valor de analisis, como algunas preposiciones,
# articulos y muletillas. 
Observaciones <- removeWords(Observaciones, words = stopwords("spanish")) 

#Quitar la puntuacion, dado que manzana y manzana. son identificados como palabras diferentes.
Observaciones <- removePunctuation(Observaciones)

# Quitar caracteres especiales del libro, como saltos de linea y tabulaciones,
# mediante expresiones regulares
Observaciones <- gsub("[[:cntrl:]]","",Observaciones)


#### Crear el corpus o documento objeto de análisis ####
Obs_Corpus <- Corpus(VectorSource(Observaciones))

#### Crear nube de palabras ####

#se mapea el Corpus como documento de texto plano usuando la funcion tm_map
Obs_mapeo <- tm_map(Obs_Corpus, PlainTextDocument)

#Creae la nube de palabras con el objeto mapeado
wordcloud(Obs_mapeo$content$content, max.words = 50, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

#Se continua con la depuración, se pueden econtrar palabras que no son de interes en el análisis
#Utilizar la funcion removeWords() indicando en el parametro "words = " que palabras se quieren eliminar de nuestro corpus o documento
Observaciones <- removeWords(Observaciones, words = c("años", "hace", "tto", "tratamiento", "control", "remito", "hoy", "ultimo", "ademas", "edad", "medico", "valoracion", "solicita", "remite", "manejo","meses"))

#Se realiza un nuevo mapero del corpus o documento
Obs_Corpus <- Observaciones %>% VectorSource() %>% Corpus()
Obs_mapeo <- Obs_Corpus %>% tm_map(PlainTextDocument)

#Se grafica de nuevo la nube
wordcloud(Obs_mapeo$content$content, max.words = 50, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))
