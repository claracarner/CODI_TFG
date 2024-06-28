library(knitr)
library(kableExtra)


taula_paisos <- data.frame(
  Ranquing = c(1, 2, 3, 4, 5, 6, 7, 7),
  Pais = c("França", "Alemanya", "Països Baixos", "Itàlia", "Espanya", "Finlàndia", "Bèlgica", "Irlanda"),
  Empreses = c(17, 14, 6, 5, 4, 2, 1, 1)
)

taula_paisos <- rbind(taula_paisos, c("", "Total", 50))

kable(taula_paisos, col.names = c("Ranquing", "Pais", "Empreses"), align = 'c') %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, 
                position = "center",
                font_size = 12) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(2, width = "10em", extra_css = "text-align: center; border-right: 1px solid black;") %>%
  column_spec(3, width = "5em", extra_css = "text-align: center;") %>%
  row_spec(0, bold = TRUE, color = "white", background = "#A4E8E0", extra_css = "border-bottom: 1px solid grey;") %>%
  row_spec(nrow(taula_paisos), bold = TRUE, color = "black", background = "#E0E0E0")

