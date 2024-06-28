# Carrega els paquets necessaris
library(knitr)
library(kableExtra)

# Crea una data frame amb les variables i les seves definicions
taula_informativa <- data.frame(
  Variable = c("Data", "Últim", "Obertura", "Màxim", "Mínim", "Volum", "Variació percentual"),
  Definició = c("El dia en què es van registrar les dades de l'índex.",
                "El valor de tancament de l'índex en aquell dia específic. Representa el preu final de l'índex en tancar la jornada borsària.",
                "El valor de l'índex a l'inici de la jornada de negociació. Indica el preu amb què es va obrir el mercat aquell dia.",
                "El valor més alt que va assolir l'índex durant la jornada de negociació.",
                "El valor més baix que va assolir l'índex durant la jornada de negociació.",
                "Indica el nombre d'accions de les empreses de l'Eurostoxx 50 que es van negociar durant la jornada.",
                "Mostra el canvi percentual del valor de l'índex respecte al valor de tancament del dia anterior.")
)

# Utilitza kable i kableExtra per crear una taula ben formatada i estèticament atractiva
kable(taula_informativa, col.names = c("Variable", "Definició"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "30em") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")


taula_variables <- data.frame(
  Variables = c("Data", "Últim", "Obertura", "Màxim", "Mínim", "Volum", "Variació percentual")
)


#per fer taula amb els noms només
taula_variables_t <- t(taula_variables)
kable(taula_variables_t, col.names = NULL, align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12)
