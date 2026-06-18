# Customização app =============================================================
# Fonte do texto
my_font <- "Roboto Condensed"
# Tema
theme_set(theme_light())
theme_projeto <- theme(
  text              = element_text(family = my_font), 
  plot.subtitle     = element_markdown(color = "#292929", face = 'bold', size = 14),
  plot.background   = element_rect("white"),
  panel.background  = element_blank(),
  axis.line         = element_line(color = "gray"),
  legend.position   = 'none',
  legend.background = element_rect(fill = "white", color = "white", linewidth = 0.5),
  axis.title        = element_text(color = "#292929", face = "bold"),
  axis.text         = element_text(color = "#292929", face = "bold"),
  axis.text.y       = element_blank(),
  axis.ticks.y      = element_blank(),
  panel.grid        = element_blank()
)

cores_vetor  <- c("#6210A1", "#24063B", "#641AA1", "#5E47ED")
cores_vetor2 <- c("#24063B", "#6210A1", "#CA96F2", "#96C1F2")