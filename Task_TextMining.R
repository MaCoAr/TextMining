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

#Quitar los numeros, pues no son relevantes para el analisis que se esta realizando
Observaciones <- removeNumbers(Observaciones)

# Quitar caracteres especiales del libro, como saltos de linea y tabulaciones,
# mediante expresiones regulares
Observaciones <- gsub("[[:cntrl:]]","",Observaciones)


#### Crear el corpus o documento objeto de análisis ####
Obs_Corpus <- Corpus(VectorSource(Observaciones))

#### Crear nube de palabras ####

# Paralabras a remover de las observaciones medicas
palabras_a_remover <- c("años", "hace", "tto", "tratamiento", "control", "remito", "hoy", "ultimo", "ademas", "edad", "medico", "valoracion", 
                        "solicita", "remite", "manejo","meses", "buena","mas", "dia","año","requiere","cita","toma","refiere","presenta","2015",
                        "normal","examenes","examen","metas","pte","dias","medicamentos","bien","urgencias","medicina","cifras","med","medicina",
                        "xxxx","igual","trae","mes","solicito","paciente","interna","habitos","tel","consultas","consulta","orden","remision",
                        "tolera","	ahora","consultado","dice","	niega","	especialista","especialistas","tipo","	cada","pendiente","cuadro",
                        "evolucion","sintomas","orden","cada","ahora","tipo","dice","viene","atencion","tel")

# Se mapea el Corpus como documento de texto plano usuando la funcion tm_map
Obs_mapeo <- tm_map(Obs_Corpus, PlainTextDocument)

# Se continua con la depuración, se pueden econtrar palabras que no son de interes en el análisis
# Utilizar la funcion removeWords() indicando en el parametro "words = " que palabras se quieren eliminar de nuestro corpus o documento
Observaciones <- removeWords(Observaciones, words = palabras_a_remover)

# Se realiza un nuevo mapeo del corpus o documento
Obs_Corpus <- Observaciones %>% VectorSource() %>% Corpus()
Obs_mapeo <- Obs_Corpus %>% tm_map(PlainTextDocument)

# Se grafica de la nube de palabras
wordcloud(Obs_mapeo$content$content, 
          max.words = 50, 
          random.order = F, 
          colors = brewer.pal(name = "Dark2", n = 8))

#### Crear matriz de terminos del documento ####

# Identificar estadisticas básicas entre palabras
obs_tdm <- TermDocumentMatrix(Obs_Corpus)

# Eliminar terminos dispersos 
obs_nuevo <- removeSparseTerms(obs_tdm, sparse = 0.95)
obs_tdm
obs_nuevo

# Crear matriz
# Transformar la TDM en una matriz estandar
obs_nuevo <- obs_nuevo %>% as.matrix()
dim(obs_nuevo)

# Obtener las sumas de los renglones (rowSums) ordenadas de mayor a menor
obs_matriz <- obs_nuevo %>% rowSums() %>% sort(decreasing = TRUE)
obs_matriz <- data.frame(palabra = names(obs_matriz), frec = obs_matriz)

#### Graficos de Frecuencias ####
obs_matriz[1:10,] %>% 
  ggplot(aes(palabra, frec)) +
  geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
  geom_text(aes(hjust = 1.3, label = frec)) +
  coord_flip() +
  labs(title = "Diez palabras más frecuentes", x = "Palabras", y = "Número de uso")

#Se obtiene una estandarización por renglones
obs_nuevo <- obs_nuevo / rowSums(obs_nuevo)

# Se obtiene una matriz de distancia, con el método de distancias euclidianas
obs_distancia <- dist(obs_nuevo, method = "euclidian")

#### Agrupapientos Jerárquicos ####

# Se usa el método de Ward (Ward.D), que es el método por defecto de la función hclust
obs_hclust <- hclust(obs_distancia, method = "ward.D")

# Ahora graficar
plot(obs_hclust, main = "Dendrograma de Niebla - hclust", sub = "", xlab = "")