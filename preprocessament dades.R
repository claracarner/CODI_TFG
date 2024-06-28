

###################TFG
#paquets 
library(kableExtra)
library(knitr)
#install.packages("tidyverse")
#install.packages("caret")
#install.packages("timechange")
# Carga las bibliotecas necesarias
library(tidyverse)
library(caret)
library(dplyr)
#install.packages("xfun")

data<-read.csv2(file="/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/diari eurstoxx, 01:01:2015_25:03:2024.csv", header=TRUE, sep="," )

#resum inicial dades
head_taula_inicial<- head(data)
taula <- kable(head_taula_inicial, caption = "Base de dades inicial Eurostoxx 50") %>%
  kable_styling(full_width = FALSE)
print(taula)

#editem les dades
data$Fecha <- as.Date(data$Fecha, format="%d.%m.%Y")

#Primer hem de treure el punt ja que sino despres no podem canviar la coma per punt com a separador
data$Último <- gsub("\\.", "", data$Último)
data$Apertura <- gsub("\\.", "", data$Apertura)
data$Máximo <- gsub("\\.", "", data$Máximo)
data$Mínimo<- gsub("\\.", "", data$Mínimo)

#de comes a punts i convertir a tipo numeric totes les columnes menys la ultima
data$Último <- as.numeric(gsub(",", ".", data$Último))
data$Apertura <- as.numeric(gsub(",", ".", data$Apertura))
data$Máximo <- as.numeric(gsub(",", ".", data$Máximo))
data$Mínimo <- as.numeric(gsub(",", ".", data$Mínimo))

# Eliminar "M" final de cada observació en la columna "Vol."
data$Vol. <- gsub("M$", "", data$Vol.) 
#convertim a millons 
data$Vol. <- as.numeric(gsub(",", ".", data$Vol.)) * 1000000



#reocordem que aquesta columna es un percentatge 
names(data)[names(data) == "X..var."] <-"variacion_porcentual_x"
data$variacion_porcentual_x<-gsub("%", "", data$variacion_porcentual_x)
data$variacion_porcentual_x<-as.numeric(gsub(",", ".", data$variacion_porcentual_x))

#canviem a catala:
variables <- c("Data", "Ultim", "Obertura", "Maxim", "Minim", "Volum", "Variacio_percentual")
names(data) <- variables


#creem taula amb resum dades
setwd("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/grafics taules")
png("resum_inicial_dades.png")
resum_dades <- kbl(summary(data), caption = "Resum estadístic de les dades") %>%
  kable_classic(full_width = F) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
dev.off()

for (variable in variables) {
  if(is.numeric(data[,variable])){
  nom <- paste(variable, ".png", sep="")
  png(nom, width = 1600, height = 1200, res = 300)
  hist(data[[variable]], main = paste(variable), xlab = variable, ylab ="Frequència" ,col = "#A4E8E0", border = "black")
  dev.off()
  }
}



###MISSINGS 
missing_values_ultimo <- sum(is.na(data$Último))
missing_values_apertura <- sum(is.na(data$Apertura))
missing_values_maximo <- sum(is.na(data$Máximo))
missing_values_minimo <- sum(is.na(data$Mínimo))
missing_values_vol <- sum(is.na(data$Vol.)) #OBSERVEM 8 MISSINGS 
missing_values_variacion <- sum(is.na(data$variacion_porcentual_x))


#IMPUTEM ELS MISSINGS AMB MICE
install.packages("mice")
library(mice)
print(head(data))

imputacio <- mice(data, method = "pmm", m = 5)

datos_imputados <- complete(imputacio)

print(head(datos_imputados))
DADES<-datos_imputados
DADES <- arrange(DADES, Data) #hem d'ordenar-ho
setwd("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES")
saveRDS(DADES, file = "DADES_pre.rds")
