# Dados Geral ==================================================================
interv_desist_ids <- df |>
  filter(
    desfecho_participante_motivo_interv %in% c("Desistência")
  ) |>
  distinct(record_id) |>
  pull()

## Elegível --------------------------------------------------
seg_particip_desist_elegi_ids <- df |> 
  filter(
    record_id %in% interv_desist_ids &
    desfecho_participante_interv == "Completou a participação"
  ) |>
  distinct(record_id) |>
  pull()

seg_particip_desist_elegi <- length(seg_particip_desist_elegi_ids)


## Exclusão -------------------------------------------------
seg_particip_desist_3m_exclusao_ids <- df |>
  filter(
    record_id %in% interv_desist_ids &
    desfecho_participante_motivo_seg_3m %in% 
      c("Óbito", "Critério de exclusão")
  ) |>
  distinct(record_id) |>
  pull()

seg_particip_desist_6m_exclusao_ids <- df |>
  filter(
    record_id %in% interv_desist_ids,
    desfecho_participante_motivo_seg_6m %in% 
      c("Óbito", "Critério de exclusão")
  ) |>
  distinct(record_id) |>
  pull()



## Realizado --------------------------------------------------
seg_particip_desist_3m_realizados_ids <- df |>
  filter(
    record_id %in% interv_desist_ids &
    redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" &
      !is.na(whodas_q1) &
      !record_id %in% seg_particip_desist_3m_exclusao_ids) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_3m_realizados <- length(seg_particip_desist_3m_realizados_ids)

seg_particip_desist_6m_realizados_ids <- df |>
  filter(
    record_id %in% interv_desist_ids &
    redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" &
      !is.na(whodas_q1) &
      !record_id %in% seg_particip_desist_6m_exclusao_ids) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_6m_realizados <- length(seg_particip_desist_6m_realizados_ids)



## Em janela -------------------------------------------------
seg_particip_desist_3m_janela_ids <- interv_andamento_df |>
  filter(
    record_id %in% seg_particip_desist_elegi_ids &
      !record_id %in% seg_particip_desist_3m_realizados_ids
  ) |>
  filter(
    (Sys.Date() - as.Date(sessao_A_data) >= 30*3 - 15) |
      (Sys.Date() - as.Date(sessao_A_data) <= 30*3 + 15)
  ) |>
  distinct(record_id) |>
  pull()

seg_particip_desist_6m_janela_ids <- interv_andamento_df |>
  filter(
    # record_id %in% seg_particip_desist_elegi_ids &
    record_id %in% seg_particip_desist_3m_realizados_ids &
      !record_id %in% seg_particip_desist_6m_realizados_ids
  ) |>
  filter(
    (Sys.Date() - as.Date(sessao_A_data) >= 30*6 - 15) |
      (Sys.Date() - as.Date(sessao_A_data) <= 30*6 + 15)
  ) |>
  distinct(record_id) |>
  pull()


## Em janela, sem contato ------------------------------------
seg_particip_desist_3m_janela_semcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" &
      record_id %in% seg_particip_desist_3m_janela_ids &
      !record_id %in% seg_particip_desist_3m_realizados &
      is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_3m_janela_semcontato <- length(seg_particip_desist_3m_janela_semcontato_ids)

seg_particip_desist_6m_janela_semcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" &
      record_id %in% seg_particip_desist_6m_janela_ids &
      !record_id %in% seg_particip_desist_6m_realizados &
      is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_6m_janela_semcontato <- length(seg_particip_desist_6m_janela_semcontato_ids)


## Em janela, com contato ------------------------------------
seg_particip_desist_3m_janela_comcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" &
      record_id %in% seg_particip_desist_3m_janela_ids &
      !record_id %in% seg_particip_desist_3m_realizados &
      !is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_3m_janela_comcontato <- length(seg_particip_desist_3m_janela_comcontato_ids)

seg_particip_desist_6m_janela_comcontato_ids <- df |>
  filter(
    redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" &
      record_id %in% seg_particip_desist_6m_janela_ids &
      !record_id %in% seg_particip_desist_6m_realizados &
      !is.na(data_tent_1_busc_seg)
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_6m_janela_comcontato <- length(seg_particip_desist_6m_janela_comcontato_ids)


## Passou da janela -------------------------------------------
seg_particip_desist_3m_passoujanela_ids <- interv_andamento_df |>
  filter(
    record_id %in% seg_particip_desist_elegi_ids &
      !record_id %in% seg_particip_desist_3m_realizados_ids
  ) |>
  filter(
    Sys.Date() - as.Date(sessao_A_data) > 30*3 + 15
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_3m_passoujanela <- length(seg_particip_desist_3m_passoujanela_ids)

seg_particip_desist_6m_passoujanela_ids <- interv_andamento_df |>
  filter(
    record_id %in% seg_particip_desist_elegi_ids &
      !record_id %in% seg_particip_desist_6m_realizados_ids
  ) |>
  filter(
    Sys.Date() - as.Date(sessao_A_data) > 30*6 + 15
  ) |>
  distinct(record_id) |>
  pull()
seg_particip_desist_6m_passoujanela <- length(seg_particip_desist_6m_passoujanela_ids)



## Dataframe geral andamento --------------------------------------------
dados_andamento_seg_particip_desist <- data.frame(
  etapa = rep(c("3 meses", "6 meses"), 4),
  var = c(
    rep("Realizados", 2), 
    rep("Em janela Com primeira tentativa de contato já preenchida", 2), 
    rep("Em janela Sem primeira tentativa de contato preenchida", 2), 
    rep("Passou da janela", 2)),
  value = c(
    seg_particip_desist_3m_realizados, seg_particip_desist_6m_realizados, 
    seg_particip_desist_3m_janela_comcontato, seg_particip_desist_6m_janela_comcontato,
    seg_particip_desist_3m_janela_semcontato, seg_particip_desist_6m_janela_semcontato,
    seg_particip_desist_3m_passoujanela, seg_particip_desist_6m_passoujanela)
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
