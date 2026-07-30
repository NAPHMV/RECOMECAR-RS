# Tipo de atendimento =========================================================
seg_manejo_tipo_df <- df |>
  filter(grepl("Arm 2:", redcap_event_name)) |>
  filter(
    if_any(
      c(atend_psico_checklist_1, atend_psiq_checklist_1, atend_assist_checklist_1), 
      \(x) x %in% "Sim")
  ) |>
  mutate(
    atend_psi = case_when(
      atend_psico_checklist_1 == "Sim" ~ "Sim",
      !is.na(atend_psico_checklist_1)  ~ "Não",
      TRUE ~ NA),
    atend_psiq = case_when(
      atend_psiq_checklist_1 == "Sim" ~ "Sim",
      !is.na(atend_psiq_checklist_1)  ~ "Não",
      TRUE ~ NA),
    atend_assist = case_when(
      atend_assist_checklist_1 == "Sim" ~ "Sim",
      !is.na(atend_assist_checklist_1)  ~ "Não",
      TRUE ~ NA)
  ) |>
  select(
    ID      = record_id,
    `Etapa` = atend_espe_psico_qnd,
    `Psicólogo Supervisor` = atend_psi, 
    `Psiquiatra`           = atend_psiq, 
    `Assistente Social`    = atend_assist
  )

seg_manejo_tipo_tabela <- seg_manejo_tipo_df |>
  select(-Etapa) |>
  pivot_longer(
    cols      = -ID,
    names_to  = "Especialista",
    values_to = "atendeu"
  ) %>% 
  filter(atendeu == "Sim") %>% 
  summarise(
    n_observado = n_distinct(ID),
    .by = Especialista
  ) %>% 
  mutate(
    perc = (n_observado / sum(n_observado, na.rm = TRUE)) * 100,
    `n (%)` = sprintf(
      "%d (%.1f%%)",
      n_observado,
      perc
    )
  ) %>% 
  select(Especialista, `n (%)`) 

seg_manejo_tipo_str <- seg_manejo_tipo_tabela |>
  arrange(`n (%)`) |>
  mutate(linha = glue("{Especialista} = {`n (%)`}")) |>
  pull(linha) |>
  paste(collapse = "\n")

