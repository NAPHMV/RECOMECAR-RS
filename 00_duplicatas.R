# IDs duplicados ==============================================================
# df |>
#   group_by(nome, telefone) |>
#   filter(n() > 1, !is.na(nome)) |> select(contains("desfecho"))

df_ids_duplicados <- df |>
  transmute(
    record_id = as.character(record_id),
    nome_key     = ifelse(!is.na(nome) & trimws(nome) != "", paste0("N_", trimws(tolower(nome))), NA),
    telefone_key = ifelse(!is.na(telefone), paste0("T_", telefone), NA)
  ) |>
  tidyr::pivot_longer(c(nome_key, telefone_key), values_to = "key") |>
  filter(!is.na(key)) |>
  select(record_id, key) |>
  graph_from_data_frame(directed = FALSE) |>
  components() |>
  (\(c) tibble(node = names(c$membership), grupo = c$membership))() |>
  semi_join(df |> transmute(node = as.character(record_id)), by = "node") |>
  group_by(grupo) |>
  filter(n() > 1) |>
  summarise(ID = paste(node, collapse = ", "), .groups = "drop") |>
  select(ID) |>
  arrange(ID)

ids_duplicados <- df |>
  transmute(
    record_id = as.character(record_id),
    nome_key     = ifelse(!is.na(nome) & trimws(nome) != "", paste0("N_", trimws(tolower(nome))), NA),
    telefone_key = ifelse(!is.na(telefone), paste0("T_", telefone), NA)
  ) |>
  tidyr::pivot_longer(c(nome_key, telefone_key), values_to = "key") |>
  filter(!is.na(key)) |>
  select(record_id, key) |>
  graph_from_data_frame(directed = FALSE) |>
  components() |>
  (\(c) tibble(node = names(c$membership), grupo = c$membership))() |>
  semi_join(df |> transmute(node = as.character(record_id)), by = "node") |>
  group_by(grupo) |>
  filter(n() > 1) |>
  distinct(node) |>
  pull()

ids_duplicados_retirar <- df |>
  filter(
    record_id %in% ids_duplicados &
      ((desfecho_participante == "Retirado" &
          desfecho_participante_motivo_exclu == "Participantes já incluídos no estudo e/ou falha de triagem") |
         (desfecho_participante_interv == "Retirado" &
            desfecho_participante_motivo_exclu_interv___3 == "Inclusão prévia no mesmo estudo ou erro identificado no processo de triagem"))
  ) |>
  pull(record_id)
append(ids_duplicados_retirar, "195")

df <- df |>
  filter(!record_id %in% ids_duplicados_retirar)

# Exports
if (F) {
  df |>
    filter(
      record_id %in% ids_duplicados &
        redcap_event_name == "Desfecho (Arm 1: Participantes)" &
        # desfecho_participante != "Retirado" &
        desfecho_participante_motivo_exclu != "Participantes já incluídos no estudo e/ou falha de triagem"
    ) |>
    select(record_id) |>
    writexl::write_xlsx("ids_duplicados_sem_desfecho_2026_08_28.xlsx")
  
  df |>
    filter(record_id %in% ids_duplicados) |>
    distinct(record_id) |>
    writexl::write_xlsx("ids_duplicados_2026_08_28.xlsx")
  
  df |>
    transmute(
      record_id = as.character(record_id),
      nome_key     = ifelse(!is.na(nome) & trimws(nome) != "", paste0("N_", trimws(tolower(nome))), NA),
      telefone_key = ifelse(!is.na(telefone), paste0("T_", telefone), NA)
    ) |>
    tidyr::pivot_longer(c(nome_key, telefone_key), values_to = "key") |>
    filter(!is.na(key)) |>
    select(record_id, key) |>
    graph_from_data_frame(directed = FALSE) |>
    components() |>
    (\(c) tibble(node = names(c$membership), grupo = c$membership))() |>
    semi_join(df |> transmute(node = as.character(record_id)), by = "node") |>
    group_by(grupo) |>
    filter(n() > 1) |>
    filter(
      node %in% c(
        df |>
          filter(
            record_id %in% ids_duplicados &
              redcap_event_name == "Desfecho (Arm 1: Participantes)" &
              # desfecho_participante != "Retirado" &
              desfecho_participante_motivo_exclu != "Participantes já incluídos no estudo e/ou falha de triagem"
          ) |>
          pull(record_id)
      )
    ) |>
    summarise(ID = paste(node, collapse = ", "), .groups = "drop") |>
    select(ID) |>
    arrange(ID)
  
  df_ids_duplicados |>
    writexl::write_xlsx("ids_duplicados_grupos_2026_08_28.xlsx")
  
  df |>
    filter(record_id %in% ids_duplicados,
           str_detect(redcap_event_name, "Desfecho")) |>
    select(record_id, desfecho_participante, desfecho_participante_motivo_exclu) |>
    writexl::write_xlsx("ids_duplicados_preenchimento_2026_08_28.xlsx")
  
}