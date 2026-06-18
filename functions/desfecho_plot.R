desfecho_plot <- function(df, plot.limits) {
  plot <- df |>
    filter(sessao != "Sessões 1 a 5") |>
    mutate(
      text = glue::glue("{sessao}\n\nMédia  = {escore_media}\nQ1-Q3 = {escore_q1} - {escore_q3}")
    ) |>
    ggplot(aes(x = sessao, y = escore_media, text = text)) +
    geom_errorbar(aes(ymin = escore_q1, ymax = escore_q3),
                  width = 0.2, color = "#10031a", alpha = 0.6) +
    geom_point(size = 3, color = "#6210A1") +
    geom_point(size = 3, color = "#6210A1") +
    geom_line(aes(group = 1), color = "#24063B") +
    scale_y_continuous(limits = plot.limits, breaks = scales::breaks_pretty()) +
    labs(
      x = NULL,
      y = "Média do Escore"
    ) +
    theme_minimal()
  plot <- ggplotly(plot, tooltip = "text")
}