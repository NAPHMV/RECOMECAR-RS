# Dados ======================================================================
## Realizados ----------------------------------------------
interv_sessoes_realiz <- interv_andamento_df |>
  select(record_id, matches("_realizada")) |>
  summarise(
    `Sessão A` = sum(sessao_A_realizada, na.rm = TRUE),
    `Sessão 1` = sum(sessao_1_realizada, na.rm = TRUE),
    `Sessão 2` = sum(sessao_2_realizada, na.rm = TRUE),
    `Sessão 3` = sum(sessao_3_realizada, na.rm = TRUE),
    `Sessão 4` = sum(sessao_4_realizada, na.rm = TRUE),
    `Sessão 5` = sum(sessao_5_realizada, na.rm = TRUE),
    `Sessão Final` = sum(sessao_final_realizada, na.rm = TRUE)
  )

interv_s1_realiz_ids <- interv_andamento_df |>
  filter(sessao_1_realizada == 1) |>
  distinct(record_id) |>
  pull()

interv_s2_realiz_ids <- interv_andamento_df |>
  filter(sessao_2_realizada == 1) |>
  distinct(record_id) |>
  pull()

interv_s3_realiz_ids <- interv_andamento_df |>
  filter(sessao_3_realizada == 1) |>
  distinct(record_id) |>
  pull()

interv_s4_realiz_ids <- interv_andamento_df |>
  filter(sessao_4_realizada == 1) |>
  distinct(record_id) |>
  pull()

interv_s5_realiz_ids <- interv_andamento_df |>
  filter(sessao_5_realizada == 1) |>
  distinct(record_id) |>
  pull()

interv_sf_realiz_ids <- interv_andamento_df |>
  filter(sessao_final_realizada == 1) |>
  distinct(record_id) |>
  pull()

## Excluídos (antigo) --------------------------------------------------
interv_sessoes_excluidos <- df |>
  filter(str_detect(redcap_event_name, "Sessao")) |>
  group_by(record_id, redcap_event_name) |>
  reframe(
    nao_agendou = enc_sa_agend_data == "Não" | enc_sessao_agend_data == "Não",
    nao_elegivel = enc_sa_superv_apto == "2 - Não" | enc_sessao_superv_apto == "Não",
    excluido = nao_agendou | nao_elegivel
  ) |>
  group_by(redcap_event_name) |>
  summarise(
    excluidos = sum(excluido, na.rm = TRUE)
  )
interv_sessoes_excluidos_ids <- df |>
  filter(str_detect(redcap_event_name, "Sessao")) |>
  group_by(record_id, redcap_event_name) |>
  reframe(
    nao_agendou = enc_sa_agend_data == "Não" | enc_sessao_agend_data == "Não",
    nao_elegivel = enc_sa_superv_apto == "2 - Não" | enc_sessao_superv_apto == "2 - Não",
    excluido = nao_agendou | nao_elegivel
  ) |>
  filter(excluido) |>
  select(record_id, redcap_event_name)


## df --------------------------------------------------
interv_andamento_resumo <- tibble(
  sessao = rep(colnames(interv_sessoes_realiz), 4),
  var = c(rep("Finalizados",7), rep("Perdas",7), rep("Aguardando sessão", 7), rep("Aguardando agendamento", 7)),
  value = c(
    as.numeric(t(interv_sessoes_realiz)),
    c(interv_sa_perda_n, interv_s1_perda_n, interv_s2_perda_n, interv_s3_perda_n,
      interv_s4_perda_n, interv_s5_perda_n, interv_sf_perda_n),
    c(interv_sa_aguardando_n, s1_aguardando_n, s2_aguardando_n, 
      s3_aguardando_n, s4_aguardando_n, 
      s5_aguardando_n, sf_aguardando_n),
    c(interv_sa_aguard_agend_n, interv_s1_aguard_agend_n, interv_s2_aguard_agend_n, 
      interv_s3_aguard_agend_n, interv_s4_aguard_agend_n, 
      interv_s5_aguard_agend_n, interv_sf_aguard_agend_n)
  )
) |>
  mutate(
    sessao = fct_relevel(as.factor(sessao), "Sessão A", "Sessão 1", "Sessão 2", "Sessão 3", "Sessão 4", "Sessão 5", "Sessão Final"),
    var    = fct_relevel(as.factor(var), "Perdas", "Aguardando agendamento", "Aguardando sessão", "Finalizados")
  )



## IDs ativos ----------------------------------------------------------
# interv_interromp_ids <- df |>
#   filter(redcap_event_name == "Agendamento (Arm 1: Participantes)",
#          sessao_agend == "Não") |>
#   pull(record_id)
# 
# interv_ativos_ids <- df |>
#   filter(
#     !(record_id %in% c(interv_sf_realiz_ids, interv_interromp_ids)) &
#       (record_id %in% interv_sa_realiz_ids)
#   ) |>
#   distinct(record_id) |>
#   pull(record_id)



# Tabela =====================================================================
interv_andamento_tabela <- interv_andamento_df |>
  select(record_id, matches("_realizada")) |>
  summarise(
    `Sessão A` = glue::glue("{sum(sessao_A_realizada, na.rm = TRUE)}/{tri_eleg_interv_n}"),
    `Sessão 1` = glue::glue("{sum(sessao_1_realizada, na.rm = TRUE)}/{sum(sessao_A_realizada, na.rm = TRUE)}"),
    `Sessão 2` = glue::glue("{sum(sessao_2_realizada, na.rm = TRUE)}/{sum(sessao_1_realizada, na.rm = TRUE)}"),
    `Sessão 3` = glue::glue("{sum(sessao_3_realizada, na.rm = TRUE)}/{sum(sessao_2_realizada, na.rm = TRUE)}"),
    `Sessão 4` = glue::glue("{sum(sessao_4_realizada, na.rm = TRUE)}/{sum(sessao_3_realizada, na.rm = TRUE)}"),
    `Sessão 5` = glue::glue("{sum(sessao_5_realizada, na.rm = TRUE)}/{sum(sessao_4_realizada, na.rm = TRUE)}"),
    `Sessão Final` = glue::glue("{sum(sessao_final_realizada, na.rm = TRUE)}/{sum(sessao_5_realizada, na.rm = TRUE)}")
  )
