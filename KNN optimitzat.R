#KNN OPTIMITZAT

library(caret)
library(tidyverse)
library(knitr)
library(kableExtra)


DADES <- readRDS("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/DADES_pib_inf.rds")

set.seed(123)

index_entrenament <- sample(nrow(DADES), 0.7 * nrow(DADES))  # Índexs aleatoris per a les dades d'entrenament
dades_entrenament <- DADES[index_entrenament, ]  # Dades d'entrenament basades en els índexs aleatoris
dades_prova <- DADES[-index_entrenament, ]  # Dades de prova basades en els índexs complementaris

variables <- c(
  "Ultim",
  "Obertura",
  "Maxim",
  "Minim",
  "Volum",
  "Variacio_percentual",
  "mes_t",
  "any_t",
  "Inflacio_eurozona",
  "PIB_varperc"
)


avaluar_model <- function(variables_seleccionades) {
  dades_entrenament_subset <- dades_entrenament[, variables_seleccionades, drop = FALSE]
  dades_prova_subset <- dades_prova[, variables_seleccionades, drop = FALSE]
  
  model <- train(
    x = dades_entrenament_subset,
    y = dades_entrenament$taxa_rendabilitat,
    method = "knn",
    trControl = ctrl,
    preProcess = c("center", "scale"),
    tuneGrid = expand.grid(k = c(1:10))
  )
  
  prediccions <- predict(model, newdata = dades_prova_subset)
  
  MSE <- mean((dades_prova$taxa_rendabilitat - prediccions)^2)
  RMSE <- sqrt(MSE)
  MAE <- mean(abs(dades_prova$taxa_rendabilitat - prediccions))
  MAPE <- mean(abs((dades_prova$taxa_rendabilitat - prediccions) / dades_prova$taxa_rendabilitat)) * 100
  
  return(list(MSE = MSE, RMSE = RMSE, MAE = MAE, MAPE = MAPE, model = model))
}


ctrl <- trainControl(method = "cv", number = 5)


resultats <- list()


for (i in 1:length(variables)) {
  combinacions <- combn(variables, i, simplify = FALSE)
  for (combinacio in combinacions) {
    resultat <- avaluar_model(combinacio)
    resultats[[paste(combinacio, collapse = ", ")]] <- resultat
  }
}


millor_combinacio_mse <- NULL
millor_mse <- Inf

millor_combinacio_rmse <- NULL
millor_rmse <- Inf

millor_combinacio_mae <- NULL
millor_mae <- Inf

millor_combinacio_mape <- NULL
millor_mape <- Inf

# Iterem sobre les combinacions de variables emmagatzemades a la llista 'resultats', per obtenir el millor
for (nom_combinacio in names(resultats)) {
  if (resultats[[nom_combinacio]]$MSE < millor_mse) {
    millor_mse <- resultats[[nom_combinacio]]$MSE
    millor_combinacio_mse <- nom_combinacio
  }
  if (resultats[[nom_combinacio]]$RMSE < millor_rmse) {
    millor_rmse <- resultats[[nom_combinacio]]$RMSE
    millor_combinacio_rmse <- nom_combinacio
  }
  if (resultats[[nom_combinacio]]$MAE < millor_mae) {
    millor_mae <- resultats[[nom_combinacio]]$MAE
    millor_combinacio_mae <- nom_combinacio
  }
  if (resultats[[nom_combinacio]]$MAPE < millor_mape) {
    millor_mape <- resultats[[nom_combinacio]]$MAPE
    millor_combinacio_mape <- nom_combinacio
  }
}



taula_resultats <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Millor_Combinacio = c(millor_combinacio_mse, millor_combinacio_rmse, millor_combinacio_mae, millor_combinacio_mape),
  Valor = c(millor_mse, millor_rmse, millor_mae, millor_mape)
)

kable(taula_resultats, col.names = c("Mètrica", "Millor Combinació", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "30em") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")





# Taula amb el model amb el millor RMSE i les seves mètriques d'error
model_millor_rmse <- resultats[[millor_combinacio_rmse]]
taula_model_millor_rmse <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Valor = c(model_millor_rmse$MSE, model_millor_rmse$RMSE, model_millor_rmse$MAE, model_millor_rmse$MAPE)
)

kable(taula_model_millor_rmse, col.names = c("Mètrica", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")
