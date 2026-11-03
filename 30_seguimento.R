# Dados Geral ==================================================================
## Elegível --------------------------------------------------
seg_particip_elegi_ids <- df |> 
  filter(desfecho_participante_interv == "Completou a participação") |>
  distinct(record_id) |>
  pull()

seg_particip_elegi <- length(seg_particip_elegi_ids)


## Exclusão -------------------------------------------------
seg_particip_3m_exclusao_ids <- df |>
  filter(
    desfecho_participante_motivo_seg_3m %in% 
      c("Óbito", "Critério de exclusão")
  ) |>
  distinct(record_id) |>
  pull()

seg_particip_6m_exclusao_ids <- df |>
  filter(
    desfecho_participante_motivo_seg_6m %in% 
      c("Óbito", "Critério de exclusão")
  ) |>
  distinct(record_id) |>
  pull()



## Realizado --------------------------------------------------
seg_particip_3m_realizados_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" &
      !is.na(whodas_q1) &
      !record_id %in% seg_particip_3m_exclusao_ids) |>
  distinct(record_id) |>
  pull()
seg_particip_3m_realizados <- length(seg_particip_3m_realizados_ids)

seg_particip_6m_realizados_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" &
      !is.na(whodas_q1) &
      !record_id %in% seg_particip_6m_exclusao_ids) |>
  distinct(record_id) |>
  pull()
seg_particip_6m_realizados <- length(seg_particip_6m_realizados_ids)



## Em janela -------------------------------------------------
seg_particip_3m_janela_ids <- interv_andamento_df |>
  filter(
    record_id %in% seg_particip_elegi_ids &
      !record_id %in% seg_particip_3m_realizados_ids
  ) |>
  filter(
    (Sys.Date() - as.Date(sessao_A_data) >= 30*3 - 15) |
      (Sys.Date() - as.Date(sessao_A_data) <= 30*3 + 15)
  ) |>
  distinct(record_id) |>
  pull()

seg_particip_6m_janela_ids <- interv_andamento_df |>
  filter(
    # record_id %in% seg_particip_elegi_ids &
    record_id %in% seg_particip_3m_realizados_ids &
      !record_id %in% seg_particip_6m_realizados_ids
  ) |>
  filter(
    (Sys.Date() - as.Date(sessao_A_data) >= 30*6 - 15) |
      (Sys.Date() - as.Date(sessao_A_data) <= 30*6 + 15)
  ) |>
  distinct(record_id) |>
  pull()


## Em janela, sem contato ------------------------------------
seg_particip_3m_janela_semcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" &
      record_id %in% seg_particip_3m_janela_ids &
      !record_id %in% seg_particip_3m_realizados &
      is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_3m_janela_semcontato <- length(seg_particip_3m_janela_semcontato_ids)

seg_particip_6m_janela_semcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" &
      record_id %in% seg_particip_6m_janela_ids &
      !record_id %in% seg_particip_6m_realizados &
      is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_6m_janela_semcontato <- length(seg_particip_6m_janela_semcontato_ids)


## Em janela, com contato ------------------------------------
seg_particip_3m_janela_comcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" &
      record_id %in% seg_particip_3m_janela_ids &
      !record_id %in% seg_particip_3m_realizados &
      !is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_3m_janela_comcontato <- length(seg_particip_3m_janela_comcontato_ids)

seg_particip_6m_janela_comcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" &
      record_id %in% seg_particip_6m_janela_ids &
      !record_id %in% seg_particip_6m_realizados &
      !is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_6m_janela_comcontato <- length(seg_particip_6m_janela_comcontato_ids)


## Passou da janela -------------------------------------------
seg_particip_3m_passoujanela_ids <- interv_andamento_df |>
  filter(
    record_id %in% seg_particip_elegi_ids &
      !record_id %in% seg_particip_3m_realizados_ids
  ) |>
  filter(
    Sys.Date() - as.Date(sessao_A_data) > 30*3 + 15
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_3m_passoujanela <- length(seg_particip_3m_passoujanela_ids)

seg_particip_6m_passoujanela_ids <- interv_andamento_df |>
  filter(
    record_id %in% seg_particip_elegi_ids &
      !record_id %in% seg_particip_6m_realizados_ids
  ) |>
  filter(
    Sys.Date() - as.Date(sessao_A_data) > 30*6 + 15
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_6m_passoujanela <- length(seg_particip_6m_passoujanela_ids)



## Dataframe geral andamento --------------------------------------------
dados_andamento_seg_particip <- data.frame(
  etapa = rep(c("3 meses", "6 meses"), 4),
  var = c(
    rep("Realizados", 2), 
    rep("Em janela Com primeira tentativa de contato já preenchida", 2), 
    rep("Em janela Sem primeira tentativa de contato preenchida", 2), 
    rep("Passou da janela", 2)),
  value = c(
    seg_particip_3m_realizados, seg_particip_6m_realizados, 
    seg_particip_3m_janela_comcontato, seg_particip_6m_janela_comcontato,
    seg_particip_3m_janela_semcontato, seg_particip_6m_janela_semcontato,
    seg_particip_3m_passoujanela, seg_particip_6m_passoujanela)
) |>
  mutate(
    etapa = fct_relevel(as.factor(etapa), "3 meses", "6 meses"),
    var = fct_relevel(
      as.factor(var), 
      "Passou da janela",
      "Em janela Sem primeira tentativa de contato preenchida", 
      "Em janela Com primeira tentativa de contato já preenchida", 
      "Realizados"),
    value = if_else(is.na(value), 0, value)
  )


# Andamento ====================================================================
seg_particip_andamento_df <- interv_andamento_df |>
  select(record_id, `Sessão A` = sessao_A_data) |>
  full_join(
    df |>
      filter(redcap_event_name == "Seguimento 3m (Arm 1: Participantes)") |>
      reframe(
        record_id = record_id,
        `3 meses` = coalesce(as.Date(whodas_20_timestamp), as.Date(phq9_dta_preenchi))
      ),
    by = "record_id"
  ) |>
  full_join(
    df |>
      filter(redcap_event_name == "Seguimento 6m (Arm 1: Participantes)") |>
      reframe(
        record_id = record_id,
        `6 meses` = coalesce(as.Date(whodas_20_timestamp), as.Date(phq9_dta_preenchi))
      ),
    by = "record_id"
  ) |>
  mutate(
    `Sessão A` = as.Date(`Sessão A`),
    prev_3m = paste0("Previsão: ", if_else(is.na(`3 meses`), `Sessão A` + 30*3, `3 meses`)),
    prev_6m = paste0("Previsão: ", if_else(is.na(`6 meses`), `Sessão A` + 30*6, `6 meses`))
  ) |>
  mutate(
    `3 meses` = as.character(`3 meses`),
    `3 meses` = coalesce(`3 meses`, prev_3m),
    `6 meses` = as.character(`6 meses`),
    `6 meses` = coalesce(`6 meses`, prev_6m),
    ano_3m    = stringr::str_extract(`3 meses`, "\\d{4}"),
    ano_6m    = stringr::str_extract(`6 meses`, "\\d{4}")
  ) |>
  select(-c(prev_3m, prev_6m)) |>
  

# Dados desfecho ===============================================================
## Perdas ----------------------------------------------------
seg_particip_3m_perda_ids <- df |>
  filter(
    desfecho_participante_motivo_seg_3m %in%
      c("Perda de acompanhamento", "Óbito", "Critério de exclusão")
  ) |>
  distinct(record_id) |> pull()
seg_particip_3m_perda_n <- length(seg_particip_3m_perda_ids)

seg_particip_6m_perda_ids <- df |>
  filter(
    desfecho_participante_motivo_seg_6m %in%
      c("Perda de acompanhamento", "Óbito", "Critério de exclusão")
  ) |>
  distinct(record_id) |> pull()
seg_particip_6m_perda_n <- length(seg_particip_6m_perda_ids)


## Desistências ----------------------------------------------
seg_particip_3m_desist_ids <- df |>
  filter(
    desfecho_participante_motivo_seg_3m %in% c("Desistência")
  ) |>
  distinct(record_id) |> pull()
seg_particip_3m_desist_n <- length(seg_particip_3m_desist_ids)

seg_particip_6m_desist_ids <- df |>
  filter(
    desfecho_participante_motivo_seg_6m %in% c("Desistência")
  ) |>
  distinct(record_id) |> pull()
seg_particip_6m_desist_n <- length(seg_particip_6m_desist_ids)

## Dataframe -------------------------------------------------
dados_seg_particip_desfecho <- df |>
  distinct(record_id) |>
  mutate(
    `3 meses` = case_when(
      record_id %in% seg_particip_3m_perda_ids      ~ "Perda",
      record_id %in% seg_particip_3m_desist_ids     ~ "Desistência",
      record_id %in% seg_particip_3m_realizados_ids ~ "Realizado",
      TRUE ~ NA
    ),
    `6 meses` = case_when(
      record_id %in% seg_particip_6m_perda_ids      ~ "Perda",
      record_id %in% seg_particip_6m_desist_ids     ~ "Desistência",
      record_id %in% seg_particip_6m_realizados_ids ~ "Realizado",
      TRUE ~ NA
    )
  ) |>
  filter(!is.na(`3 meses`) | !is.na(`6 meses`))


# Tabelas ======================================================================
tabela_andamento_seg_particip <- tibble(
  `3 meses` = glue::glue("{seg_particip_3m_realizados}/{seg_particip_elegi}"),
  `6 meses` = glue::glue("{seg_particip_6m_realizados}/{seg_particip_3m_realizados}")
)
