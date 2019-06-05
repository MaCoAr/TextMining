#### URLS de ayuda ####
# https://stackoverflow.com/questions/5171593/r-memory-management-cannot-allocate-vector-of-size-n-mb
# https://community.rstudio.com/t/error-cannot-allocate-vector-of-size-76-4-gb/10615/3

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

#Estructura del libro
str(Archivo_OAM)
# 
# dim(Archivo_OAM)
# texto_OAM <- as.matrix()
# texto_OAM <- Archivo_OAM %>% select(Archivo_OAM) %>% as.matrix
# dim(texto)

# estructura_OAM <- Archivo_OAM %>% data.frame()

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

#Quitar los numeros, pues no son relevantes para el analisis que se esta realizando
Observaciones <- removeNumbers(Observaciones)

# Quitar caracteres especiales del libro, como saltos de linea y tabulaciones,
# mediante expresiones regulares
Observaciones <- gsub("[[:cntrl:]]","",Observaciones)


#### Crear el corpus o documento objeto de análisis ####
Obs_Corpus <- Corpus(VectorSource(Observaciones))

#### Crear nube de palabras ####

# Se mapea el Corpus como documento de texto plano usuando la funcion tm_map
Obs_mapeo <- tm_map(Obs_Corpus, PlainTextDocument)

# Crear la nube de palabras con el objeto mapeado
wordcloud(Obs_mapeo$content$content, max.words = 50, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

# Se continua con la depuración, se pueden econtrar palabras que no son de interes en el análisis
# Utilizar la funcion removeWords() indicando en el parametro "words = " que palabras se quieren eliminar de nuestro corpus o documento
palabras_a_remover <- c("años", "hace", "tto", "tratamiento", "control", "remito", "hoy", "ultimo", "ademas", "edad", "medico", "valoracion", 
                        "solicita", "remite", "manejo","meses", "buena","mas", "dia","año","requiere","cita","toma","refiere","presenta","2015",
                        "normal","examenes","examen","metas","pte","dias","medicamentos","bien","urgencias","medicina","cifras","med","medicina",
                        "xxxx","igual","trae","mes","solicito","paciente","interna")
Observaciones <- removeWords(Observaciones, words = palabras_a_remover )

# Se realiza un nuevo mapero del corpus o documento
Obs_Corpus <- Observaciones %>% VectorSource() %>% Corpus()
Obs_mapeo <- Obs_Corpus %>% tm_map(PlainTextDocument)

# Se grafica de nuevo la nube
wordcloud(Obs_mapeo$content$content, 
          max.words = 50, 
          random.order = F, 
          colors = brewer.pal(name = "Dark2", n = 8))

#### Crear matriz de terminos del documento ####

# Identificar estadisticas básicas entre palabras
obs_tdm <- TermDocumentMatrix(Obs_Corpus)
obs_tdm

# Transformar la TDM en una matriz estandar
gc(reset = TRUE)
obs_matriz <- as.matrix(obs_tdm[["dimnames"]])
dim(obs_matriz)

# Obtener las sumas de los renglones (rowSums) ordenadas de mayor a menor
obs_matriz <- obs_matriz %>% rowSums() %>% sort(decreasing = TRUE)
obs_matriz <- data.frame(palabra = names(obs_matriz), frec = obs_matriz)

# Con este objeto (data frame) tambien se puede crear una nube de palabras
wordcloud(words = obs_matriz$palabra, freq = obs_matriz$frec, max.words = 50, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))