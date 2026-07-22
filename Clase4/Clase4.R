#######################################################################################
################      CLASE 4                     ####################################
################  MCD Carlos Castro Correa        #####################################
#######################################################################################
#######################################################################################

#Contenido: Métricas de clasificación, modelos ML regresión y métricas

#Paquetes hasta el momento:
library(readr) #leer archivos, por ejemplo csv
library(tidyverse) #estructura de datos
library(rpart) #arboles de decisón
library(rpart.plot) #graficos de arbol de decision
library(readxl) #leer archivos de excel
library(tidymodels) #Función fit para arboles




################################################################################
################################################################################
##########                Netflix                        #######################
################################################################################
################################################################################

####################################################################
######  Singular Value Descomposition
#https://en.wikipedia.org/wiki/Singular_value_decomposition

#Leemos una BD con películas y usuarios
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_julio2026/Clase4")
netflix <- read_excel("peli_netflix.xlsx")

#Observamos los datos
View(netflix)

#Conservamos solo los datos

#Peliculas: columnas
#Usuarios: registros
netflix <- netflix[2:nrow(netflix),2:ncol(netflix)]

#Cambiar nombres de columnas para apreciar mejor los registros
colnames(netflix) <- paste0("peli_", seq_len(ncol(netflix)))
#Cambiar nombres de filas para apreciar mejor los registros
rownames(netflix) <- paste0("user_", seq_len(nrow(netflix)))

#Observamos de nueva cuenta los datos
View(netflix)

#Número de peliculas y usuarios
dim(netflix)
  #348 usuarios
  #30 películas

#Alpicamos SDV a la matriz de datos
svd <- svd(netflix)

#Obtenemos las tres matrices de la descomposición
svd$d

svd$u

svd$v

#Matrices para cálculos Netflix
p <-  svd$u * sqrt(svd$d)
q <-  sqrt(svd$d) * t(svd$v)

#Con estos datos y algunas operaciones matriciales sería posible estimar 
#calificaciones de películas de usuarios que nos las han visto... (con cierto error...ECM)...
dim(p)# 30*30
dim(q)# 348*30















################################################################################
##########      Algoritmos ML Regresión                  #######################
################################################################################

#Datos rendimiento de alumnos
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_julio2026/Clase4")
performance <- read_csv("Student_Performance.csv")

#Veamos qué contiene la base de datos
performance %>% 
  head() %>% 
  View()


####################################################################
#Regresión lineal
#El modelo clásico e inicial de siempre...
reg_model <- lm(Performance_Index  ~ ., data=performance)

#Parámetros de la recta (modelo base de la regresión)
reg_model

#Predicciones
pred <- predict(object = reg_model,newdata = performance)

#Observamos las estimaciones del modelo:
pred

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred

#Observamos los datos
View(performance)


####################################################################
#Para calcular el ECM de nuestros modelos de regresión
# vamos a programar una función en R que me permita evaluar todos los modelos

#ECM
A <- performance$Performance_Index
B <- pred #Resutaldos del modelo de predicción

#Con esta notación podemos crear una función para utilizarla siempre
ecm <- function(A,B){
  error <- 0
  for (i in 1:length(B)){
    error <- error + ((A[i] - B[i])^2)
  }
  error <- error/length(B)
  return(error)
}

#Por supuesto, hay funciones de R que también lo calculan pero 
#el objetivo es crear el código para entender muy bien la fórmula

#Finalmente, utilizamos la fórmula para calcular el ECM regresión
ecm_regresion <- ecm(A,B)
ecm_regresion



#Originalmente, estudiamos problemas de clasificación y sus modelos,
#pero recuerda... hay algunos algoritmos que funcionan para ambos tipos de problema...



####################################################################
####################################################################
#Árbol de decisión

#Leemos los datos nuevamente
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_julio2026/Clase4")
performance <- read_csv("Student_Performance.csv")

#Observa que es la MISMA fórmula que para problemas de clasificación
tree <- rpart(Performance_Index ~ ., data = performance)

#Imprimimos el modelo de ML
tree

#¿Qué variables y rangos está tomando nuestro arbol?
rpart.plot(tree)

#Predicciones con el árbol de regresion
pred_arbol <- predict(object = tree,newdata = performance)

#Imprime el modelo ML:
pred_arbol

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred_arbol

#Observamos los datos
View(performance)

#Pregunta Importante:
#¿Qué paso adicional tendríamos que hacer en un árbol para clasificación?...

#ECM árbol
ecm_arbol <- ecm(A,pred_arbol)
ecm_arbol

#Pregunta:
  #Intuitivamente, ¿se te ocurre alguna razón por la que el modelo de regresión funciona mejor que un árbol de decisión?








####################################################################
####################################################################
#Bosque aleatorio

#Leemos los datos nuevamente
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_julio2026/Clase4")
performance <- read_csv("Student_Performance.csv")

#Corremos el modelo bosque aleatorio para problemas de regresión
model_rf <- caret::train( #Estoy espeficicando que quiero usar la funcion train() del paquete caret...
  Performance_Index ~ .,
  data = performance,
  method = "rf",
  ntree = 5,#Comenzamos simulando con 5 árboles
)

#Observemos el modelo:
model_rf

#Predicciones sobre el mismo conjunto de datos
pred_bosque <- predict(object = model_rf,newdata = performance)

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred_bosque

#Observamos los datos
View(performance)

#ECM árbol
ecm_bosque_5 <- ecm(A,pred_bosque)
ecm_bosque_5



#Pregunta: ¿Qué pasaría si aumentamos a 10 árboles?...

#Leemos los datos nuevamente
setwd("~/Desktop/Carlos/cursos/ITAM/Diplomado_ML_julio2026/Clase4")
performance <- read_csv("Student_Performance.csv")

#Corremos el modelo bosque aleatorio para problemas de regresión
model_rf <- caret::train(
  Performance_Index ~ .,
  data = performance,
  method = "rf",
  ntree = 10,#Aumentamos a 25 árboles
)

#Observamos el modelo 
model_rf

#Predicciones
pred_bosque <- predict(object = model_rf,newdata = performance)

#Las agregamos en la tabla performance para comparar
performance$prediccion <- pred_bosque

#Observamos los datos
View(performance)

#ECM árbol
ecm_bosque_10 <- ecm(A,pred_bosque)
ecm_bosque_10

#¿Cuál fue mejor regresión lineal, bosque o árbol?
ecm_regresion

ecm_bosque_5

ecm_bosque_10


####################################################################
#Preguntas:

#1. ¿Cuál corrió más lento? ¿por qué?

#2. ¿Cómo toda la decisión del ECM un bosque aleatorio? ¿por qué?... #Regresar a Diapositivas
