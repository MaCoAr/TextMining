#### Instalando y cargando paquetes ####
# Prerequisitos de los paquetes a utilizar
install.packages('NLP', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(NLP)
install.packages('RColorBrewer', dependencies=TRUE, repos='http://cran.rstudio.com/')
library(RColorBrewer)

#Instalar
install.packages('tm', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('wordcloud', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('ggplot2', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('readr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('cluster', dependencies=TRUE, repos='http://cran.rstudio.com/')

# Cargar
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)


#### Inicio el proceso de data mining ####

#### Cargar el libro y relizarle transformaciones ####
#Cargar el libro
lineaInicial <- 790
lineaFinal <- 6631
libro <- read_lines(file.choose(),skip = lineaInicial, n_max = lineaFinal - lineaInicial)

#Estructura del libro
str(libro)

#Saber la cantidad de caracteres maximo por linea
linea_max <- max(nchar(libro[0:length(libro)]))

#Crear parrafos y reestructura el libro
diez <- rep(1:ceiling(length(libro)/10), each = 10)
#Se ajusta la cantidad de parrafos para la longitud real del libro, pasando de 4850 a 4841
diez <- diez[1:length(libro)]
#Convertir el libro en un data frame
estructuraLibro <- cbind(diez, libro) %>% data.frame()

#Unir las lineas de texto para armar los parrafos, se realiza de la siguiente forma:
#cada parrafo calculado (variable diez) 
#se uniran de a diez filas (paste) para con ello armar el parrafo separando.
#las filas por espacio en blanco (collapse)
texto <- aggregate(formula = libro~diez, data = estructuraLibro, FUN = paste, collapse = " ")

#Se transforma la variable (texto) en una matriz para facilitar los pasos siguientes
dim(texto)
texto <- texto %>% select(libro) %>% as.matrix
dim(texto)


#### Limpieza de Texto ####
#Quitar caracteres especiales del libro, como saltos de linea y tabulaciones,

#mediante expresiones regulares
texto <- gsub("[[:cntrl:]]","",texto)

#Convertir todo a minusculas
texto <- tolower(texto)

#Eliminar palabras vacias, aquellas con poco valor de analisis, como algunas preposiciones,
#articulos y muletillas. 
texto <- removeWords(texto, words = stopwords("spanish")) 

#Quitar la puntuacion, dado que manzana y manzana. son identificados como palabras diferentes.
texto <- removePunctuation(texto)

#Quitar los numeros, pues no son relevantes para el analisis que se esta realizando
texto <- removeNumbers(texto)

#Quitar los espacios en blanco de exceso
texto <- stripWhitespace(texto)



#### Crear el corpus o documento de objeto de analisis ####
mil_corpus <- Corpus(VectorSource(texto)) 
# mil_corpus # Esta compuesto por 585 documentos o parrafos

#Crear la nube de palabras
#se mapea el Corpus como documento de texto plano usuando la funcion tm_map
mil_mapeo <- tm_map(mil_corpus, PlainTextDocument)

#Ahora se crea la nube de palabras
wordcloud(mil_mapeo$content$content, max.words = 100, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

#Se continua con la depuracion, se pueden econtrar palabras que no son de interes aun en el analisis
#Se utiliza la funcion removeWords() indicando en el parametro "words = " que se quieren eliminar de nuestro corpus o documento
texto <- removeWords(texto, words = c("entonces", "tal", "tan", "así", "aquí", "dijo", "pues"))

#Se realiza un nuevo mapero del corpus o documento
mil_corpus <- texto %>% VectorSource() %>% Corpus()
mil_mapeo <- mil_corpus %>% tm_map(PlainTextDocument)

#Se grafica de nuevo la nube
wordcloud(mil_mapeo$content$content, max.words = 100, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

#### Crear matriz de terminos del documento ####
#Permite identificar estadisticas basicas entre palabras
#El texto tiene 7299 palabras diferentes
mil_tdm <- TermDocumentMatrix(mil_corpus)
mil_tdm

#### Frecuencia de palabras ####
#Transformar la TDM en una matriz estandar
mil_matriz <- as.matrix(mil_tdm)
dim(mil_matriz)

# Obtener las sumas de los renglones (rowSums) ordenadas de mayor a menor
mil_matriz <- mil_matriz %>% rowSums() %>% sort(decreasing = TRUE)
mil_matriz <- data.frame(palabra = names(mil_matriz), frec = mil_matriz)

#con este objeto (data frame) tambien se puede crear una nube de palabras
wordcloud(words = mil_matriz$palabra, freq = mil_matriz$frec, max.words = 100, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))

#### Graficos de Frecuencias ####
mil_matriz[1:12,] %>% 
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) +
  coord_flip() +
  labs(title = "Doce palabras mas frecuentes", x = "Palabras", y = "Numero de uso")
  
#Utilizar la funcion mutate de dplyr para obtener el porcentaje de uso
mil_matriz %>% 
  mutate(perc = (frec/sum(frec))*100) %>% 
  .[1:10,] %>% 
  ggplot(aes(palabra,perc)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = round(perc,2))) +
  coord_flip() +
  labs(title = "Las diez palabras más frecuentes", x = "Palabras", y = "Porcentaje de uso")


#### Eliminar terminos dispersos ####
mil_nuevo <- removeSparseTerms(mil_tdm, sparse = 0.95)
mil_tdm
mil_nuevo

#### Matriz de distancias ####
#Se transforma la matriz de términos (distancias) a un objeto de tipo "matrix" para realizar las operaciones posteriores
mil_nuevo <- mil_nuevo %>% as.matrix()

#Se obtiene una estandarización por renglones
mil_nuevo <- mil_nuevo / rowSums(mil_nuevo)

# Se obtiene una matriz de distancia, con el método de distancias euclidianas
mil_distancia <- dist(mil_nuevo, method = "euclidian")

#### Agrupapientos Jerárquicos ####
# Se usa el método de Ward (Ward.D), que es el método por defecto de la función hclust
mil_hclust <- hclust(mil_distancia, method = "ward.D")
# Ahora graficar
plot(mil_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")
#### FIN ejecicio ####
