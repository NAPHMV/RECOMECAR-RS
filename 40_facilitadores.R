# Consentimento ================================================================
facilit_seg_andamento_consent <- df |>
  filter(
    redcap_event_name == "Baseline (Arm 2: Facilitadores)" &
      tcle_consentiu_questionarios == "Sim"
  ) |>
  reframe(ID = record_id, Consentimento = as.Date(data_preenchi_facili))


# Geral =======================================================================
facilit_seg_andamento_resumo <- df |>
  filter(
    record_id %in% facilit_seg_andamento_consent$ID &
      redcap_event_name %in% c(
        "Seguimento 3m (Arm 2: Facilitadores)",
        "Seguimento 6m (Arm 2: Facilitadores)",
        "Seguimento 9m (Arm 2: Facilitadores)",
        "Seguimento 12m (Arm 2: Facilitadores)"
      )) |>
  select(record_id, redcap_event_name, facilit_segue_estudo, whodas_20_timestamp) |>
  mutate(
    redcap_event_name   = paste(str_extract(redcap_event_name, "\\d+"), "meses"),
    redcap_event_name = factor(
      redcap_event_name,
      levels = c("3 meses", "6 meses", "9 meses", "12 meses")
    ),
    whodas_20_timestamp = case_when(
      facilit_segue_estudo == "Sim" & 
        !is.na(whodas_20_timestamp) ~ as.character(as.Date(whodas_20_timestamp)),
      facilit_segue_estudo == "Sim" &
        is.na(whodas_20_timestamp)  ~ "Indisponível",
      facilit_segue_estudo == "Não" ~ "Saiu do estudo",
      TRUE ~ NA
    )
  ) |>
  pivot_wider(id_cols = 'record_id', names_from = 'redcap_event_name', 
              values_from = 'whodas_20_timestamp', names_expand = TRUE) |>
  mutate(
    `6 meses`  = if_else(`3 meses` == "Saiu do estudo", "", `3 meses`),
    `9 meses`  = if_else(`6 meses` == "Saiu do estudo" | `6 meses` == "", "", `9 meses`),
    `12 meses` = if_else(`9 meses` == "Saiu do estudo" | `9 meses` == "", "", `12 meses`)
  ) |>
  rename(ID = record_id)
