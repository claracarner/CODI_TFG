#gbm optimitzat 2

DADES_pib_inf <- readRDS("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/DADES_pib_inf.rds")

library(gbm)
library(caret)
library(knitr)
library(kableExtra)

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

# Funció per avaluar el model amb una combinació de variables utilitzant validació creuada i un valor fix de n.trees
avaluar_model_gbm <- function(variables_seleccionades, ntree) {
  set.seed(12345)
  particions <- createFolds(DADES_pib_inf$taxa_rendabilitat, k = num_folds, list = TRUE, returnTrain = FALSE)
  
  MSE_GBM <- RMSE_GBM <- MAE_GBM <- MAPE_GBM <- numeric(num_folds)
  
  for (i in 1:num_folds) {
    dades_entrenament <- DADES_pib_inf[-particions[[i]], c(variables_seleccionades, "taxa_rendabilitat"), drop = FALSE]
    dades_prova <- DADES_pib_inf[particions[[i]], c(variables_seleccionades, "taxa_rendabilitat"), drop = FALSE]
    
    model_GBM <- gbm(taxa_rendabilitat ~ ., data = dades_entrenament, 
                     distribution = "gaussian", 
                     n.trees = ntree, 
                     interaction.depth = 4)
    
    prediccions_GBM <- predict(model_GBM, newdata = dades_prova, n.trees = ntree)
    
    MSE_GBM[i] <- mean((dades_prova$taxa_rendabilitat - prediccions_GBM)^2)
    RMSE_GBM[i] <- sqrt(MSE_GBM[i])
    MAE_GBM[i] <- mean(abs(dades_prova$taxa_rendabilitat - prediccions_GBM))
    MAPE_GBM[i] <- mean(abs((dades_prova$taxa_rendabilitat - prediccions_GBM) / dades_prova$taxa_rendabilitat)) * 100
  }
  
  return(list(
    MSE = mean(MSE_GBM), RMSE = mean(RMSE_GBM),
    MAE = mean(MAE_GBM), MAPE = mean(MAPE_GBM)
  ))
}

# Pas 1: Trobar la millor combinació de variables utilitzant un valor fix de n.trees
ntree_fix_GBM <- 100  # Valor fix per a la primera fase

resultats_variables_GBM <- list()

for (i in 1:length(variables)) {
  combinacions <- combn(variables, i, simplify = FALSE)
  for (combinacio in combinacions) {
    resultats_comb_GBM <- avaluar_model_gbm(combinacio, ntree_fix_GBM)
    resultats_variables_GBM[[paste(combinacio, collapse = ", ")]] <- resultats_comb_GBM
  }
}

# Seleccionar la millor combinació de variables basada en totes les mètriques
millor_combinacio_mse_GBM <- NULL
millor_mse_GBM <- Inf
millor_combinacio_rmse_GBM <- NULL
millor_rmse_GBM <- Inf
millor_combinacio_mae_GBM <- NULL
millor_mae_GBM <- Inf
millor_combinacio_mape_GBM <- NULL
millor_mape_GBM <- Inf

for (nom_combinacio in names(resultats_variables_GBM)) {
  if (resultats_variables_GBM[[nom_combinacio]]$MSE < millor_mse_GBM) {
    millor_mse_GBM <- resultats_variables_GBM[[nom_combinacio]]$MSE
    millor_combinacio_mse_GBM <- nom_combinacio
  }
  if (resultats_variables_GBM[[nom_combinacio]]$RMSE < millor_rmse_GBM) {
    millor_rmse_GBM <- resultats_variables_GBM[[nom_combinacio]]$RMSE
    millor_combinacio_rmse_GBM <- nom_combinacio
  }
  if (resultats_variables_GBM[[nom_combinacio]]$MAE < millor_mae_GBM) {
    millor_mae_GBM <- resultats_variables_GBM[[nom_combinacio]]$MAE
    millor_combinacio_mae_GBM <- nom_combinacio
  }
  if (resultats_variables_GBM[[nom_combinacio]]$MAPE < millor_mape_GBM) {
    millor_mape_GBM <- resultats_variables_GBM[[nom_combinacio]]$MAPE
    millor_combinacio_mape_GBM <- nom_combinacio
  }
}

# Resultats finals per a les mètriques abans de provar amb diferents arbres
taula_resultats_inicial_GBM <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Millor_Combinacio = c(millor_combinacio_mse_GBM, millor_combinacio_rmse_GBM, millor_combinacio_mae_GBM, millor_combinacio_mape_GBM),
  Valor = c(millor_mse_GBM, millor_rmse_GBM, millor_mae_GBM, millor_mape_GBM)
)

kable(taula_resultats_inicial_GBM, col.names = c("Mètrica", "Millor Combinació", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "30em") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")

# Pas 2: Optimitzar el nombre d'arbres (ntree) per a la millor combinació de variables basada en RMSE
variables_seleccionades_GBM <- strsplit(millor_combinacio_rmse_GBM, ", ")[[1]]
ntree_values_GBM <- c(500, 995, 2000)

resultats_ntree_GBM <- list()
for (ntree in ntree_values_GBM) {
  resultats_ntree_GBM[[paste("ntree", ntree, sep = "_")]] <- avaluar_model_gbm(variables_seleccionades_GBM, ntree)
}

# Crear una taula amb els diferents valors de RMSE per als diferents nombres d'arbres
taula_rmse_per_ntree_GBM <- data.frame(
  Ntree = ntree_values_GBM,
  RMSE = sapply(resultats_ntree_GBM, function(x) x$RMSE)
)

kable(taula_rmse_per_ntree_GBM, col.names = c("Nombre d'arbres (ntree)", "RMSE"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")

# Seleccionar el millor valor de ntree basat en RMSE
millor_ntree_GBM <- NULL
millor_rmse_ntree_GBM <- Inf

for (ntree in names(resultats_ntree_GBM)) {
  if (resultats_ntree_GBM[[ntree]]$RMSE < millor_rmse_ntree_GBM) {
    millor_rmse_ntree_GBM <- resultats_ntree_GBM[[ntree]]$RMSE
    millor_ntree_GBM <- ntree
  }
}

# Resultats finals després d'optimitzar ntree
taula_resultats_final_GBM <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Valor = c(resultats_ntree_GBM[[millor_ntree_GBM]]$MSE, resultats_ntree_GBM[[millor_ntree_GBM]]$RMSE, resultats_ntree_GBM[[millor_ntree_GBM]]$MAE, resultats_ntree_GBM[[millor_ntree_GBM]]$MAPE)
)

kable(taula_resultats_final_GBM, col.names = c("Mètrica", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")

# Resultats finals amb ntree = 995
resultats_ntree_995_GBM <- resultats_ntree_GBM[["ntree_995"]]
taula_resultats_995_GBM <- data.frame(
  Metrica = c("MSE", "RMSE", "MAE", "MAPE"),
  Valor = c(resultats_ntree_995_GBM$MSE, resultats_ntree_995_GBM$RMSE, resultats_ntree_995_GBM$MAE, resultats_ntree_995_GBM$MAPE)
)

kable(taula_resultats_995_GBM, col.names = c("Mètrica", "Valor"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")
