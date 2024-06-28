
DADES<- readRDS("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/DADES_pre.rds")
setwd("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES")
#install.packages("gbm")
library(gbm)
#install.packages("lubridate")
library(lubridate) #Per treballar amb dates
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

##preparar base de dades per a poder predir a 3 mesos vista (90 dies)#######
#sumem 90 dies ja que sino, si fem 3 mesos hi han problemes quan la data es 31

DADES$Data <- as.Date(DADES$Data)  # Assegurar que la columna de dates està en format de data
DADES$Data_a_90dies <- as.Date(DADES$Data) + 90
DADES2 <- DADES %>% select(Data, Obertura_90dies = Obertura)
DADES <- left_join(DADES, DADES2, by = c("Data_a_90dies" = "Data"))

#pels caps de setmana no tenim dades, eliminem totes les obertures de + 90 dies
#que cauen a dies de cap de setmana
DADES<- DADES[!is.na(DADES$Obertura_90dies), ]

#hem de separaho per dia mes i any pq sino pel model no ens serveix, pero la var que ens interessa és la Obertura de  +90 dies com a resposta 
DADES$Dia_t90 <- day(DADES$Data_a_90dies)
DADES$Mes_t90 <- month(DADES$Data_a_90dies)
DADES$Any_t90 <- year(DADES$Data_a_90dies)


#volem nova variable objectiu, ja que el volum a 90 dies no te sentit. necesitem taxa rendibilitat per poder comparar 
#Farem (volum futur( +90) - volum actual ) / volum actual 
DADES$taxa_rendabilitat<- ((DADES$Obertura_90dies - DADES$Obertura) / DADES$Obertura)*100

#guardem noves DADES amb data a 90 dies i variable objectiu+
saveRDS(DADES, file = "DADES.rds")


#AFEGIM VARIABLE INFLACIO

#necesitem mes_t i any_t de Dades per poder ajuntar la variable inflacio (nomes la tenim mensual)
#install.packages("lubridate")
library(lubridate)
DADES$mes_t <- month(DADES$Data)
DADES$any_t <- year(DADES$Data)
base_de_dades<-kable(head(DADES[,13:15]), caption = "Primeres files de la base de dades") %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  column_spec(1, bold = TRUE) 

#carreguem dades inflació 
inflacio_eurozona <- read.csv2("/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/explicatives/explicatives_csv/evolucio_inflacion_en_la_eurozona (còpia).csv", header=TRUE, sep=",")
taula_inflacio <- head(inflacio_eurozona)

kable(taula_inflacio, caption = "Primeres files de la base de dades inflacio_eurozona") %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  column_spec(1, bold = TRUE)  

#necessitem que mesos siguin homogenis per ajuntar la inflacio a les dades 
convertir_mes_a_numero <- function(datos) {
  datos$Periodo <- tolower(datos$Periodo) #per convertir a minuscula 
  datos$Periodo <- ifelse(datos$Periodo == "enero", 1,
                    ifelse(datos$Periodo == "febrero", 2,
                    ifelse(datos$Periodo == "marzo", 3,
                    ifelse(datos$Periodo == "abril", 4,
                    ifelse(datos$Periodo == "mayo", 5,
                    ifelse(datos$Periodo == "junio", 6,
                    ifelse(datos$Periodo == "julio", 7,
                    ifelse(datos$Periodo == "agosto", 8,
                    ifelse(datos$Periodo == "septiembre", 9,
                    ifelse(datos$Periodo == "octubre", 10,
                    ifelse(datos$Periodo == "noviembre", 11,
                    ifelse(datos$Periodo == "diciembre", 12,
                    NA))))))))))))
  return(datos)
}

inflacio_eurozona <- convertir_mes_a_numero(inflacio_eurozona)
inflacio_eurozona$Año <- as.numeric(inflacio_eurozona$Año)
names(inflacio_eurozona)[names(inflacio_eurozona) == "Inflación.de.la.zona.euro"] <- "Inflacio_eurozona"

#ajuntem inflacio a cada obseravcio en base a mes i any corresponent
DADES3<- left_join(DADES, inflacio_eurozona, by = c("mes_t" = "Periodo", c("any_t" = "Año"))  )

##AFEGIM VARIABLE PIB

#Preparem base de dades PIB (esta per trimestres, falta afegir mesos per poder enganxar)
PIB_0<-read.csv2(file = "/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/explicatives/explicatives_csv/variacion_anual_del_pib_de_la_eurozona.csv", header=TRUE, sep=",")
#PIB_0<-as.data.frame(head(PIB_0))
kable(PIB_0, caption = "Primeres files de la base de dades del PIB") %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  column_spec(1, bold = TRUE)  

names(PIB_0)[names(PIB_0) == "X."] <- "PIB_varperc"
trimestre_mes<-read.csv2(file = "/Users/claracarnermarsal/Desktop/segon semestre 5e/TFG /necessari/DADES/altres/relacio_trimestre_mes.csv", header=TRUE, sep=";")

kable(head(trimestre_mes), caption = "Correspondència  mes amb trimestre") %>%
  kable_styling(bootstrap_options = "striped", full_width = F) %>%
  column_spec(1, bold = TRUE)

class(trimestre_mes$Periodo)
class(PIB_0$Periodo)

PIB<-left_join(PIB_0, trimestre_mes, by = c( "Periodo" = "Periodo" )) #ara ja tenim pib mensual 

#Ajuntem amb DADES per mes i any
DADES3<- left_join(DADES3, PIB, by = c("mes_t" = "mes", c("any_t" = "Año")) )
DADES_pib_inf <- subset(DADES3, select = -Periodo)
DADES_pib_inf$Inflacio_eurozona <- as.numeric(DADES_pib_inf$Inflacio_eurozona)
DADES_pib_inf$PIB_varperc<- as.numeric(DADES_pib_inf$PIB_varperc)
saveRDS(DADES_pib_inf, file = "DADES_pib_inf.rds")
#ens tornem a guardar base de dades ja que  la farem servir per modelitzar

kable(head(DADES_pib_inf), caption = "Mostra base de dades amb variables afegides") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

tipus_de_columnes <- sapply(DADES, class)
print(tipus_de_columnes)

