# tengo que sacar la Interacción entre AF con amigos y participación en competiciones (lo tengo al principio de basura)

rm(list = ls())

library(ggplot2)
library(randomForest)
library(tidyverse)
library(caret)
library(pROC)



# Load the data

data <- read.csv("~/Desktop/Scripts/Eskola_adina/Random forest/Input/dataframe_datuak.csv", sep = ";", stringsAsFactors = TRUE)



# Prepare the data --------------------------------------------------------


# Remove some columns (not needed)

# data  <-  data |> select(-c(MEDEA, IT_5A, IT_5B))


# select the data

data <- data |> 
  select(c(Urtea, Biztanlegoa_A, Biztanlegoa_B, Ikastetxe.Mota, Generoa, IT_1, IT_2, IT_3, IT_4, IT_6, IT_7A, IT_7B, IT_8A, IT_8B, IT_11, IT_12, IT_13))


# Assign the levels to the factors

data[data$Urtea == 1, ]$Urtea <- "2022-2023"
data[data$Urtea == 2, ]$Urtea <- "2023-2024"

data$Urtea <- as.factor(data$Urtea)

data[data$Biztanlegoa_A == 1, ]$Biztanlegoa_A <- "menos de 1000"
data[data$Biztanlegoa_A == 2, ]$Biztanlegoa_A <- "1000-5000"
data[data$Biztanlegoa_A == 3, ]$Biztanlegoa_A <- "5001-10000"
data[data$Biztanlegoa_A == 4, ]$Biztanlegoa_A <- "10001-20000"
data[data$Biztanlegoa_A == 5, ]$Biztanlegoa_A <- "20001-50000"
data[data$Biztanlegoa_A == 6, ]$Biztanlegoa_A <- "50001-100000"
data[data$Biztanlegoa_A == 7, ]$Biztanlegoa_A <- "más de 100000"

data$Biztanlegoa_A <- as.factor(data$Biztanlegoa_A)

data[data$Biztanlegoa_B == 1, ]$Biztanlegoa_B <- "menos de 5000"
data[data$Biztanlegoa_B == 2, ]$Biztanlegoa_B <- "5000-40000"
data[data$Biztanlegoa_B == 3, ]$Biztanlegoa_B <- "más de 40000"

data$Biztanlegoa_B <- as.factor(data$Biztanlegoa_B)

data[data$Ikastetxe.Mota == 2, ]$Ikastetxe.Mota <- "Pribatua"
data[data$Ikastetxe.Mota == 1, ]$Ikastetxe.Mota <- "Publikoa"


data$Ikastetxe.Mota <- as.factor(data$Ikastetxe.Mota)

data[data$Generoa == 1,]$Generoa <- "Neska"
data[data$Generoa == 2,]$Generoa <- "Mutila"
data[data$Generoa == 3,]$Generoa <- "Ez bitarra"
data[data$Generoa == 4,]$Generoa <- "nahiago dut ez erantzun"

data$Generoa <- as.factor(data$Generoa)

data[data$IT_1 == 1, ]$IT_1 <- "Bai"
data[data$IT_1 == 2, ]$IT_1 <- "Ez"

data$IT_1 <- as.factor(data$IT_1)

data[data$IT_2 == 1, ]$IT_2 <- "Bai"
data[data$IT_2 == 2, ]$IT_2 <- "Ez"

data$IT_2 <- as.factor(data$IT_2)

data[data$IT_3 == 1, ]$IT_3 <- "Bai"
data[data$IT_3 == 2, ]$IT_3 <- "Ez"

data$IT_3 <- as.factor(data$IT_3)

data[data$IT_4 == 1, ]$IT_4 <- "Bai"
data[data$IT_4 == 2, ]$IT_4 <- "Ez"

data$IT_4 <- as.factor(data$IT_4)

data[data$IT_6 == 1, ]$IT_6 <- "Bai"
data[data$IT_6 == 2, ]$IT_6 <- "Ez"

data$IT_6 <- as.factor(data$IT_6)

data[data$IT_7A == 1, ]$IT_7A <- "Oinez"
data[data$IT_7A == 2, ]$IT_7A <- "Garraio aktiboa"
data[data$IT_7A == 3, ]$IT_7A <- "Garraio motor"

data$IT_7A <- as.factor(data$IT_7A)

data[data$IT_7B == 1, ]$IT_7B <- "Oinez"
data[data$IT_7B == 2, ]$IT_7B <- "Garraio aktiboa"
data[data$IT_7B == 3, ]$IT_7B <- "Garraio motor"

data$IT_7B <- as.factor(data$IT_7B)


data[data$IT_8A == 1, ]$IT_8A <- "2 baino gutxiago"
data[data$IT_8A == 2, ]$IT_8A <- "3-4 bitartean"
data[data$IT_8A == 3, ]$IT_8A <- "4 baino gehiago"

data$IT_8A <- as.factor(data$IT_8A)

data[data$IT_8B == 1, ]$IT_8B <- "2 baino gutxiago"
data[data$IT_8B == 2, ]$IT_8B <- "3-4 bitartean"
data[data$IT_8B == 3, ]$IT_8B <- "4 baino gehiago"

data$IT_8B <- as.factor(data$IT_8B)



data[data$IT_11 == 1, ]$IT_11 <- "inoiz ez"
data[data$IT_11 == 2, ]$IT_11 <- "batzuetan"
data[data$IT_11 == 3, ]$IT_11 <- "askotan"
data[data$IT_11 == 4, ]$IT_11 <- "beti"

data$IT_11 <- as.factor(data$IT_11)

data[data$IT_12 == 1, ]$IT_12 <- "inoiz ez"
data[data$IT_12 == 2, ]$IT_12 <- "batzuetan"
data[data$IT_12 == 3, ]$IT_12 <- "askotan"
data[data$IT_12 == 4, ]$IT_12 <- "beti"

data$IT_12 <- as.factor(data$IT_12)

data[data$IT_13 == 1, ]$IT_13 <- "Bai"
data[data$IT_13 == 2, ]$IT_13 <- "Ez"

data$IT_13 <- as.factor(data$IT_13)



# Rename the columns

data <- data |> 
  rename(
    dia_de_AF = IT_1,
    AF_no_organizada = IT_2,
    AF_organizada = IT_3,
    Participa_en_multideporte = IT_4,
    Participa_en_competiciones = IT_6, 
    Desplazamiento_casa_centro_escolar = IT_7A, 
    Desplazamiento_centro_escolar_casa = IT_7B, 
    Pantalla_recreativa_entre_semana = IT_8A, 
    Pantalla_recreativa_fin_de_semana = IT_8B,
    Practica_AF_con_familiares = IT_11,
    Practica_AF_con_amigos_as = IT_12,  
    Animar_amigos_as_para_practicar = IT_13  
  )




#Is it balanced?
prop.table(table(data$dia_de_AF))

#smote
library(DMwR)
set.seed(1234)
data_balanced <- SMOTE(dia_de_AF ~ ., data = data, perc.over = 100, perc.under = 200)


# How is balanced now?
prop.table(table(data_balanced$dia_de_AF))



# Spliting into training and testing set ----------------------------------

# Split the data into training and testing sets
set.seed(1234)
train_index <- createDataPartition(data_balanced$dia_de_AF, p = 0.7, list = FALSE)
train <- data_balanced[train_index, ]
test <- data_balanced[-train_index, ]


# How many observations are in the training and testing sets?
nrow(train)
nrow(test)


# Training the model ------------------------------------------------------
set.seed(1234)
model <- randomForest(dia_de_AF ~ ., data = train, ntree = 1000, importance = TRUE)
model


# Predict the test data 
pred <- predict(model, newdata = test)


# Confusion matrix with the predicted data 
confusionMatrix(pred, test$dia_de_AF)


#ROC and AUC 
pred_prob <- predict(model, newdata = test, type = "prob")

# Cargar la librería pROC si no está cargada
library(pROC)

# Asumiendo que "Bai" es la clase positiva en `dia_de_AF`
roc_curve <- roc(test$dia_de_AF, pred_prob[, "Bai"])  # Cambia "Bai" por el nombre de tu clase positiva
auc_value <- auc(roc_curve)
print(auc_value)

# Graficar la curva ROC
plot(roc_curve, main = paste("Curva ROC - AUC:", round(auc_value, 3)), col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, lwd = 2, col = "red")





# Optimizing the mtry using tuneRF ----------------------------------------- no lo he utilizado porque el OOB es ligeramente peor que el anterior. 
# optimal_mtry <- tuneRF(train[, -1], train[, 1], ntreeTry = 100, stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE)

# Seleccionar el mejor valor de mtry
# best_mtry <- optimal_mtry[which.min(optimal_mtry[, 2]), 1]

# Entrenar el modelo de Random Forest con el mtry óptimo
# set.seed(1234)
# final_model <- randomForest(dia_de_AF ~ ., data = train, mtry = best_mtry, ntree = 1000, importance = TRUE)
# final_model






# Retrain the model will all the data (train and test) --------------
set.seed(1234)
model_train_test <- randomForest(dia_de_AF ~ ., data = data_balanced, ntree = 1000, importance = TRUE)
model_train_test

# Importance of the variables (train and test) 
var_importance_train_test <- importance(model_train_test)
print(var_importance_train_test)
varImpPlot(model_train_test, main = "Importancia de Variables")


# Crear dataframe con importancia de variables
importance_df_train_test <- data.frame(
  Variable = rownames(var_importance_train_test),
  MeanDecreaseAccuracy = var_importance_train_test[,3],
  MeanDecreaseGini = var_importance_train_test[,4]
)

# Graph of the importance of the variables for the model (train and test)
ggplot(importance_df_train_test, aes(x=reorder(Variable, MeanDecreaseAccuracy), 
                                     y=MeanDecreaseAccuracy)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title="Importancia de Variables para la precisión del modelo",
       x="Variables",
       y="Mean Decrease Accuracy")




# Analysis of the results --------------------------------------------------------

## Contingency tables -------

variables <- c("Participa_en_competiciones", 
               "AF_no_organizada", 
               "AF_organizada",
               "Practica_AF_con_amigos_as",
               "Practica_AF_con_familiares", 
               "Biztanlegoa_A",
               "Animar_amigos_as_para_practicar",
               "Desplazamiento_casa_centro_escolar",
               "Pantalla_recreativa_fin_de_semana",
               "Desplazamiento_centro_escolar_casa",
               "Generoa",
               "Participa_en_multideporte",
               "Ikastetxe.Mota",
               "Urtea",
               "Pantalla_recreativa_entre_semana",
               "Biztanlegoa_B"
)

for(var in variables) {
  # Eliminar NAs solo para este análisis específico
  datos_completos <- na.omit(data[c(var, "dia_de_AF")])
  
  # Crear tabla y verificar si hay datos
  if(nrow(datos_completos) > 0) {
    tabla <- table(datos_completos[[var]], datos_completos[["dia_de_AF"]])
    
    # Imprimir resultados
    cat("\nProporciones para", var, ":\n")
    cat("Número de casos válidos:", nrow(datos_completos), "\n")
    cat("Casos excluidos por NA:", nrow(data) - nrow(datos_completos), "\n")
    print(tabla)
    cat("\nPorcentajes por fila:\n")
    print(round(prop.table(tabla, 1) * 100, 2))
    cat("\n-------------------\n")
  } else {
    cat("\nNo hay datos suficientes para", var, "\n")
  }
}



# Differences between Bai and Ez for dia_de_AD in each variable of the vector 
variables <- c("Participa_en_competiciones", 
               "AF_no_organizada", 
               "AF_organizada",
               "Practica_AF_con_amigos_as",
               "Practica_AF_con_familiares", 
               "Biztanlegoa_A",
               "Animar_amigos_as_para_practicar",
               "Desplazamiento_casa_centro_escolar",
               "Pantalla_recreativa_fin_de_semana",
               "Desplazamiento_centro_escolar_casa",
               "Generoa",
               "Participa_en_multideporte",
               "Ikastetxe.Mota",
               "Urtea",
               "Pantalla_recreativa_entre_semana",
               "Biztanlegoa_B"
)

# Crear un data frame para almacenar las diferencias
diferencias <- data.frame(
  Variable = character(),
  Diferencia = numeric(),
  stringsAsFactors = FALSE
)

for(var in variables) {
  # Eliminar NAs solo para este análisis específico
  datos_completos <- na.omit(data[c(var, "dia_de_AF")])
  
  if(nrow(datos_completos) > 0) {
    # Crear tabla de proporciones
    tabla <- table(datos_completos[[var]], datos_completos[["dia_de_AF"]])
    proporciones <- prop.table(tabla, 1) * 100  # Convertir a porcentajes
    
    # Calcular diferencias (Bai - Ez) para cada categoría de la variable
    diferencias_var <- proporciones[, "Bai"] - proporciones[, "Ez"]
    
    # Añadir resultados al data frame
    diferencias_nuevas <- data.frame(
      Variable = paste(var, names(diferencias_var)),
      Diferencia = diferencias_var,
      stringsAsFactors = FALSE
    )
    diferencias <- rbind(diferencias, diferencias_nuevas)
  }
}

# Ordenar diferencias de mayor a menor
diferencias_ordenadas <- diferencias[order(-diferencias$Diferencia), ]

# Mostrar resultados
cat("\nDiferencias entre Bai y Ez (ordenadas de mayor a menor):\n")
print(diferencias_ordenadas)

# Opcional: Crear un gráfico de barras
library(ggplot2)

ggplot(diferencias_ordenadas, aes(x = reorder(Variable, Diferencia), y = Diferencia)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Voltear el gráfico para mejor visualización
  theme_minimal() +
  labs(
    title = "Diferencia en porcentajes entre Bai y Ez",
    x = "Variables",
    y = "Diferencia (Bai - Ez) en porcentaje"
  ) +
  theme(axis.text.y = element_text(size = 8))  # Ajustar tamaño del texto si es necesario




## Gráficos de efectos parciales --------

# Los gráficos de efectos parciales que muestran cómo cada variable afecta la probabilidad de predicción "Bai"
# En los gráficos de efectos parciales: Las tendencias de cómo cada variable afecta la probabilidad de predicción "Bai". Esto nos mostrará si hay una relación positiva o negativa y qué tan fuerte es.
par(mfrow=c(4,4)) # Para mostrar varios gráficos juntos
partialPlot(model_train_test, data_balanced, "Participa_en_competiciones", "Bai",
            main="Efecto de Participación en competiciones")
partialPlot(model_train_test, data_balanced, "AF_no_organizada", "Bai",
            main="Efecto de AF no organizada")
partialPlot(model_train_test, data_balanced, "Practica_AF_con_amigos_as", "Bai",
            main="Efecto de AF con amigos")
partialPlot(model_train_test, data_balanced, "Practica_AF_con_familiares", "Bai",
            main="Efecto de AF con familiares")
partialPlot(model_train_test, data_balanced, "AF_organizada", "Bai",
            main="Efecto de AF organizada")
partialPlot(model_train_test, data_balanced, "Biztanlegoa_A", "Bai",
            main="Efecto de Tamaño de población")
partialPlot(model_train_test, data_balanced, "Animar_amigos_as_para_practicar", "Bai",
            main="Efecto de Animar a amigos/as")
partialPlot(model_train_test, data_balanced, "Desplazamiento_casa_centro_escolar", "Bai",
            main="Efecto de Desplazamiento casa-centro escolar")
partialPlot(model_train_test, data_balanced, "Pantalla_recreativa_fin_de_semana", "Bai",
            main="Efecto de Tiempo de pantalla recreativa")
partialPlot(model_train_test, data_balanced, "Desplazamiento_centro_escolar_casa", "Bai",
            main="Efecto de Desplazamiento centro escolar-casa")
partialPlot(model_train_test, data_balanced, "Generoa", "Bai",
            main="Efecto de Género")
partialPlot(model_train_test, data_balanced, "Participa_en_multideporte", "Bai",
            main="Efecto de Participación en multideporte")
partialPlot(model_train_test, data_balanced, "Ikastetxe.Mota", "Bai",
            main="Efecto de Tipo de centro escolar")
partialPlot(model_train_test, data_balanced, "Urtea", "Bai",
            main="Efecto de Año académico")
partialPlot(model_train_test, data_balanced, "Pantalla_recreativa_entre_semana", "Bai",
            main="Efecto de Tiempo de pantalla recreativa")
partialPlot(model_train_test, data_balanced, "Biztanlegoa_B", "Bai",
            main="Efecto de Tamaño de población")





# Interaction analysis-------------------------

# Interacción entre AF con amigos y participación en competiciones
interaction_plot <- table(data$Practica_AF_con_amigos_as, 
                          data$Participa_en_competiciones,
                          data$dia_de_AF)
# Cargar librerías necesarias
library(dplyr)
library(tidyr)

# Convertir la tabla en un data frame
df <- as.data.frame(interaction_plot)

# Renombrar columnas para facilitar la manipulación
colnames(df) <- c("Practica_AF_con_amigos_as", "Participa_en_competiciones", "dia_de_AF", "Freq")

# Calcular proporciones dentro de cada combinación de Practica_AF_con_amigos_as y Participa_en_competiciones
df <- df %>%
  group_by(Practica_AF_con_amigos_as, Participa_en_competiciones) %>%
  mutate(Proporcion = Freq / sum(Freq) * 100) %>%
  ungroup()

# Convertir de nuevo a formato tabla para visualizar mejor
tabla_final <- df %>%
  select(-Freq) %>%
  spread(key = dia_de_AF, value = Proporcion)

# Imprimir la tabla resultante
print(tabla_final)













print("Interacción AF amigos x Competiciones:")
prop.table(interaction_plot, margin=1)





















































# BASURA -------


# Interacción entre AF con amigos y participación en competiciones
interaction_plot <- table(data$Practica_AF_con_amigos_as, 
                          data$Participa_en_competiciones,
                          data$dia_de_AF)
print("Interacción AF amigos x Competiciones:")
print(prop.table(interaction_plot, margin=c(1,2)))




par(mfrow=c(1,1)) # Restaurar configuración original



-----
  ## Gráficos de efectos parciales para todas las variables ----
# Lista de variables predictoras
predictores <- c("Participa_en_competiciones", 
                 "AF_no_organizada", 
                 "AF_organizada",
                 "Practica_AF_con_amigos_as",
                 "Practica_AF_con_familiares", 
                 "Biztanlegoa_A",
                 "Animar_amigos_as_para_practicar",
                 "Desplazamiento_casa_centro_escolar",
                 "Pantalla_recreativa_fin_de_semana",
                 "Desplazamiento_centro_escolar_casa",
                 "Generoa",
                 "Participa_en_multideporte",
                 "Ikastetxe.Mota",
                 "Urtea",
                 "Pantalla_recreativa_entre_semana",
                 "Biztanlegoa_B")

# Crear un diccionario de nombres más descriptivos
nombres_descriptivos <- c(
  "Participa_en_competiciones" = "Participación en competiciones deportivas",
  "AF_no_organizada" = "Actividad física no organizada",
  "AF_organizada" = "Actividad física organizada",
  "Practica_AF_con_amigos_as" = "Práctica de actividad física con amigos/as",
  "Practica_AF_con_familiares" = "Práctica de actividad física con familiares",
  "Biztanlegoa_A" = "Tamaño de población (detallado)",
  "Animar_amigos_as_para_practicar" = "Animar a amigos/as a practicar actividad física",
  "Desplazamiento_casa_centro_escolar" = "Modo de desplazamiento de casa al centro escolar",
  "Pantalla_recreativa_fin_de_semana" = "Tiempo de pantalla recreativa en fin de semana",
  "Desplazamiento_centro_escolar_casa" = "Modo de desplazamiento del centro escolar a casa",
  "Generoa" = "Género",
  "Participa_en_multideporte" = "Participación en multideporte",
  "Ikastetxe.Mota" = "Tipo de centro escolar",
  "Urtea" = "Año académico",
  "Pantalla_recreativa_entre_semana" = "Tiempo de pantalla recreativa entre semana",
  "Biztanlegoa_B" = "Tamaño de población (agrupado)"
)

# Configurar el dispositivo gráfico para múltiples gráficos
par(mfrow = c(4, 4), mar = c(4, 4, 3, 1))  # 4x4 grid, ajusta los márgenes

# Crear los partial plots para cada variable
for(var in predictores) {
  partialPlot(model_train_test, 
              data_balanced,  # o train, dependiendo de qué datos quieras usar
              var, 
              "Bai",
              main = nombres_descriptivos[var],
              xlab = "Valores de la variable",
              ylab = "Probabilidad AF diaria")
}

# Restaurar la configuración original del dispositivo gráfico
par(mfrow = c(1, 1))

# Opcionalmente, guardar en PDF
pdf("efectos_parciales.pdf", width = 15, height = 15)
par(mfrow = c(4, 4), mar = c(4, 4, 3, 1))
for(var in predictores) {
  partialPlot(model_train_test, 
              data_balanced,  # o train
              var, 
              "Bai",
              main = nombres_descriptivos[var],
              xlab = "Valores de la variable",
              ylab = "Probabilidad AF diaria")
}
dev.off()

# Si prefieres ver los gráficos uno por uno, puedes usar:
for(var in predictores) {
  partialPlot(model_train_test, 
              data_balanced,  # o train
              var, 
              "Bai",
              main = nombres_descriptivos[var],
              xlab = "Valores de la variable",
              ylab = "Probabilidad AF diaria")
  # Pausa para ver cada gráfico (presiona Enter para continuar)
  readline(prompt = "Presiona Enter para ver el siguiente gráfico...")
}




















# 3. DISTRIBUCIÓN DE PREDICCIONES
# En los diagramas de caja: La distribución de las probabilidades predichas, lo que nos ayudará a ver si hay diferencias claras entre las categorías de cada variable.
# Diagramas de caja que muestran la distribución de las probabilidades predichas según los valores de cada variable importante
# Obtener probabilidades predichas
pred_prob <- predict(model, newdata = test, type = "prob")

# Crear gráficos de caja para cada variable importante
par(mfrow=c(2,3))

?boxplot()
boxplot(pred_prob[,2] ~ test$Participa_en_competiciones, 
        main="Prob. de día de AF según\nparticipación en competiciones",
        ylab="Probabilidad de día de AF")
boxplot(pred_prob[,2] ~ test$AF_no_organizada,
        main="Prob. de día de AF según\nAF no organizada",
        ylab="Probabilidad de día de AF")
boxplot(pred_prob[,2] ~ test$Practica_AF_con_amigos_as,
        main="Prob. de día de AF según\nAF con amigos",
        ylab="Probabilidad de día de AF")
boxplot(pred_prob[,2] ~ test$Practica_AF_con_familiares,
        main="Prob. de día de AF según\nAF con familiares",
        ylab="Probabilidad de día de AF")
boxplot(pred_prob[,2] ~ test$AF_organizada,
        main="Prob. de día de AF según\nAF organizada",
        ylab="Probabilidad de día de AF")
par(mfrow=c(1,1))








# Veamos si hay efectos sinérgicos entre las variables más importantes

# Interacción entre AF con amigos y participación en competiciones
interaction_plot <- table(data$Practica_AF_con_amigos_as, 
                          data$Participa_en_competiciones,
                          data$dia_de_AF)
print("Interacción AF amigos x Competiciones:")
print(prop.table(interaction_plot, margin=c(1,2)))


# 3. IMPORTANCIA CONDICIONAL
# Comparar la importancia de las variables cuando se controlan otras
library(party)
cforest_model <- cforest(dia_de_AF ~ ., data=train, controls=cforest_unbiased(ntree=1000))
varimp <- varimp(cforest_model)
print("Importancia condicional de variables:")
print(sort(varimp, decreasing=TRUE))


# En RStudio, instala y carga el paquete usethis si no lo tienes
install.packages("usethis")
library(usethis)

# Crear un token de GitHub
usethis::create_github_token()

# Guarda el token en tus credenciales
library(gitcreds)
gitcreds::gitcreds_set()
