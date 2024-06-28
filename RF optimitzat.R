#RF OPTIMITZAT

library(randomForest)
library(caret)
library(knitr)
library(kableExtra)

DADES_pib_inf <- readRDS("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/DADES_pib_inf.rds")

num_folds <- 5


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

# Funció per avaluar el model amb una combinació de variables utilitzant validació creuada 
avaluar_model_rf <- function(variables_seleccionades, ntree) {
  set.seed(12345)
  particions <- createFolds(DADES_pib_inf$taxa_rendabilitat, k = num_folds, list = TRUE, returnTrain = FALSE)
  
  MSE_RF <- RMSE_RF <- MAE_RF <- MAPE_RF <- numeric(num_folds)
  
  for (i in 1:num_folds) {
    dades_entrenament <- DADES_pib_inf[-particions[[i]], c(variables_seleccionades, "taxa_rendabilitat"), drop = FALSE]
    dades_prova <- DADES_pib_inf[particions[[i]], c(variables_seleccionades, "taxa_rendabilitat"), drop = FALSE]
    
    model_RF <- randomForest(taxa_rendabilitat ~ ., data = dades_entrenament, ntree = ntree)
    
    prediccions_RF <- predict(model_RF, newdata = dades_prova)
    
    MSE_RF[i] <- mean((dades_prova$taxa_rendabilitat - prediccions_RF)^2)
    RMSE_RF[i] <- sqrt(MSE_RF[i])
    MAE_RF[i] <- mean(abs(dades_prova$taxa_rendabilitat - prediccions_RF))
    MAPE_RF[i] <- mean(abs((dades_prova$taxa_rendabilitat - prediccions_RF) / dades_prova$taxa_rendabilitat)) * 100
  }
  
  return(list(
    MSE = mean(MSE_RF), RMSE = mean(RMSE_RF),
    MAE = mean(MAE_RF), MAPE = mean(MAPE_RF)
  ))
}

# Pas 1: Trobar la millor combinació de variables utilitzant un valor fix de ntree
ntree_fix <- 100  # Valor fix per a la primera fase

resultats_variables_RF <- list()

for (i in 1:length(variables)) {
  combinacions <- combn(variables, i, simplify = FALSE)
  for (combinacio in combinacions) {
    resultats_comb_RF <- avaluar_model_rf(combinacio, ntree_fix)
    resultats_variables_RF[[paste(combinacio, collapse = ", ")]] <- resultats_comb_RF
  }
}

# Seleccionar la millor combinació de variables basada en totes les mètriques
millor_combinacio_mse_RF <- NULL
millor_mse_RF <- Inf
millor_combinacio_rmse_RF <- NULL
millor_rmse_RF <- Inf
millor_combinacio_mae_RF <- NULL
millor_mae_RF <- Inf
millor_combinacio_mape_RF <- NULL
millor_mape_RF <- Inf

for (nom_combinacio in names(resultats_variables_RF)) {
  if (resultats_variables_RF[[nom_combinacio]]$MSE < millor_mse_RF) {
    millor_mse_RF <- resultats_variables_RF[[nom_combinacio]]$MSE
    millor_combinacio_mse_RF <- nom_combinacio
  }
  if (resultats_variables_RF[[nom_combinacio]]$RMSE < millor_rmse_RF) {
    millor_rmse_RF <- resultats_variables_RF[[nom_combinacio]]$RMSE
    millor_combinacio_rmse_RF <- nom_combinacio
  }
  if (resultats_variables_RF[[nom_combinacio]]$MAE < millor_mae_RF) {
    millor_mae_RF <- resultats_variables_RF[[nom_combinacio]]$MAE
    millor_combinacio_mae_RF <- nom_combinacio
  }
  if (resultats_variables_RF[[nom_combinacio]]$MAPE < millor_mape_RF) {
    millor_mape_RF <- resultats_variables_RF[[nom_combinacio]]$MAPE
    millor_combinacio_mape_RF <- nom_combinacio
  }
}

# Resultats finals per a les mètriques abans de provar amb diferents arbres
taula_resultats_inicial_RF <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Millor_Combinacio = c(millor_combinacio_mse_RF, millor_combinacio_rmse_RF, millor_combinacio_mae_RF, millor_combinacio_mape_RF),
  Valor = c(millor_mse_RF, millor_rmse_RF, millor_mae_RF, millor_mape_RF)
)

kable(taula_resultats_inicial_RF, col.names = c("Mètrica", "Millor Combinació", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "30em") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")

# Pas 2: Optimitzar el nombre d'arbres (ntree) per a la millor combinació de variables basada en RMSE
variables_seleccionades_RF <- strsplit(millor_combinacio_rmse_RF, ", ")[[1]]
ntree_values_RF <- c(200, 500, 995)

resultats_ntree_RF <- list()
for (ntree in ntree_values_RF) {
  resultats_ntree_RF[[paste("ntree", ntree, sep = "_")]] <- avaluar_model_rf(variables_seleccionades_RF, ntree)
}

# Taula amb els diferents valors de RMSE per als diferents nombres d'arbres
taula_rmse_per_ntree <- data.frame(
  Ntree = ntree_values_RF,
  RMSE = sapply(resultats_ntree_RF, function(x) x$RMSE)
)

kable(taula_rmse_per_ntree, col.names = c("Nombre d'arbres (ntree)", "RMSE"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")

# Seleccionar el millor valor de ntree basat en RMSE
millor_ntree_RF <- NULL
millor_rmse_ntree_RF <- Inf

for (ntree in names(resultats_ntree_RF)) {
  if (resultats_ntree_RF[[ntree]]$RMSE < millor_rmse_ntree_RF) {
    millor_rmse_ntree_RF <- resultats_ntree_RF[[ntree]]$RMSE
    millor_ntree_RF <- ntree
  }
}

# Resultats finals després d'optimitzar ntree
taula_resultats_final_RF <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Valor = c(resultats_ntree_RF[[millor_ntree_RF]]$MSE, resultats_ntree_RF[[millor_ntree_RF]]$RMSE, resultats_ntree_RF[[millor_ntree_RF]]$MAE, resultats_ntree_RF[[millor_ntree_RF]]$MAPE)
)

kable(taula_resultats_final_RF, col.names = c("Mètrica", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")

