#taula caracteristiques mercats financers 


library(knitr)
library(kableExtra)

taula_informativa <- data.frame(
  Variable = c("Transparència", "Llibertat", "Profunditat", "Amplitud", "Flexibilitat"),
  Definició = c("És essencial que la informació sobre els actius sigui accessible i clara per a tots els participants del mercat. Això inclou la disponibilitat de dades actualitzades sobre els preus, volums de transacció i altres informacions rellevants per a la presa de decisions.",
                "Fa referència a la no limitació per a l'accés de compradors i venedors, permetent una lliure formació de preus sense influències externes. Això implica l'absència de barreres d'entrada o sortida i la possibilitat de negociar actius en les quantitats desitjades.",
                "Un mercat és profund quan pot absorbir grans ordres de compra o venda sense afectar significativament el preu dels actius. Això significa que hi ha suficients compradors i venedors per mantenir l'estabilitat dels preus davant de grans volums de transaccions.",
                "Fa referència a la varietat d'actius que es negocien en un mercat. Un mercat ampli ofereix una àmplia gamma d'actius financers diferents, el que permet als inversors diversificar les seves carteres i reduir els riscos.",
                "És la capacitat del mercat per reaccionar ràpidament davant canvis en els preus dels actius. Un mercat flexible ajusta els preus ràpidament quan es detecten desajustos, permetent que els preus reflecteixin ràpidament la informació nova.")
)

kable(taula_informativa, col.names = c("Variable", "Definició"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "30em", extra_css = "text-align: justify;") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;")



