# Intervenção ==================================================================
## Encaminhados =========================================================
interv_manejo_sa_enc <- df |>
  filter(
    redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)",
    !is.na(enc_sa_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sa_atend_ind)

interv_manejo_s1_enc <- df |>
  filter(
    redcap_event_name == "Sessao 1 (Arm 1: Participantes)",
    !is.na(enc_sessao_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sessao_atend_ind)

interv_manejo_s2_enc <- df |>
  filter(
    redcap_event_name == "Sessao 2 (Arm 1: Participantes)",
    !is.na(enc_sessao_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sessao_atend_ind)

interv_manejo_s3_enc <- df |>
  filter(
    redcap_event_name == "Sessao 3 (Arm 1: Participantes)",
    !is.na(enc_sessao_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sessao_atend_ind)

interv_manejo_s4_enc <- df |>
  filter(
    redcap_event_name == "Sessao 4 (Arm 1: Participantes)",
    !is.na(enc_sessao_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sessao_atend_ind)

interv_manejo_s5_enc <- df |>
  filter(
    redcap_event_name == "Sessao 5 (Arm 1: Participantes)",
    !is.na(enc_sessao_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sessao_atend_ind)

interv_manejo_sf_enc <- df |>
  filter(
    redcap_event_name == "Sessao final (Arm 1: Participantes)",
    !is.na(enc_sessao_atend_ind)
  ) |>
  select(record_id, redcap_event_name, encaminhado = enc_sessao_atend_ind)


interv_manejo_enc <- bind_rows(
  interv_manejo_sa_enc, interv_manejo_s1_enc, interv_manejo_s2_enc, interv_manejo_s3_enc, interv_manejo_s4_enc,
  interv_manejo_s5_enc, interv_manejo_sf_enc
)

interv_manejo_algum_enc_n <- interv_manejo_enc |>
  # pivot_wider(id_cols = "record_id", names_from = "redcap_event_name", values_from = "encaminhado") |>
  # filter(if_any(`Sessao de apresentação (Arm 1: Participantes)`:`Sessao final (Arm 1: Participantes)`, 
  #               ~ . == "Sim")) |>
  # distinct(record_id) |>
  filter(encaminhado == "Sim") |>
  distinct(record_id) |>
  nrow()

interv_manejo_enc_n <- interv_manejo_enc |>
  # pivot_wider(id_cols = "record_id", names_from = "redcap_event_name", values_from = "encaminhado") |>
  # filter(if_any(`Sessao de apresentação (Arm 1: Participantes)`:`Sessao final (Arm 1: Participantes)`, 
  #               ~ . == "Sim")) |>
  # distinct(record_id) |>
  filter(encaminhado == "Sim") |>
  nrow()




## Aguardando atendimento ===============================================
interv_manejo_aguard_atend_n <- df |>
  filter(
    enc_sessao_superv_apto == "3 - Aguardando atendimento especializado" |
      enc_sa_superv_apto == "3 - Aguardando atendimento especializado") |>
  distinct(record_id) |>
  nrow()
interv_manejo_aguard_atend_str <- glue::glue(
  "{interv_manejo_aguard_atend_n}/{interv_manejo_enc_n} ({round(100*interv_manejo_aguard_atend_n/interv_manejo_enc_n, 2)} %)"
)


## Perdas ================================================================
### Falta de retorno ==============================================
interv_manejo_retorno_n <- df |>
  filter(
    redcap_event_name != "Triagem (Arm 1: Participantes)" &
      (if_any(c(atend_psiq_comp_atend_1,atend_psiq_comp_atend_2,
                atend_psiq_comp_atend_3,atend_psiq_comp_atend_4),
              \(x) x == "Não atendeu")) |
      (if_any(c(atend_assist_comp_atend_1,atend_assist_comp_atend_2,
                atend_assist_comp_atend_3,atend_assist_comp_atend_4),
              \(x) x == "Não atendeu")) |
      (if_any(c(atend_psico_comp_atend_1,atend_psico_comp_atend_2,
                atend_psico_comp_atend_3,atend_psico_comp_atend_4),
              \(x) x == "Não atendeu"))
  ) |>
  distinct(record_id) |>
  nrow()
interv_manejo_retorno_str <- glue(
  "{interv_manejo_retorno_n}/{interv_manejo_enc_n} ({round(100*interv_manejo_retorno_n/interv_manejo_enc_n, 2)} %)"
)

### Desistência ===================================================
interv_manejo_desist_n <- df |>
  filter(
    enc_sa_agend_data == "Não" | enc_sessao_agend_data == "Não"
  ) |>
  distinct(record_id) |>
  nrow()
interv_manejo_desist_str <- glue(
  "{interv_manejo_desist_n}/{interv_manejo_enc_n} ({round(100*interv_manejo_desist_n/interv_manejo_enc_n, 2)} %)"
)



## Algum atend ===================================================
interv_manejo_algum_realiz_n <- df |>
  filter(
    redcap_event_name != "Triagem (Arm 1: Participantes)" &
      (if_any(c(atend_psiq_comp_atend_1,atend_psiq_comp_atend_2,
                atend_psiq_comp_atend_3,atend_psiq_comp_atend_4),
              \(x) x == "Sim e realizou atendimento")) |
      (if_any(c(atend_assist_comp_atend_1,atend_assist_comp_atend_2,
                atend_assist_comp_atend_3,atend_assist_comp_atend_4),
              \(x) x == "Sim e realizou atendimento")) |
      (if_any(c(atend_psico_comp_atend_1,atend_psico_comp_atend_2,
                atend_psico_comp_atend_3,atend_psico_comp_atend_4),
              \(x) x == "Sim e realizou atendimento"))
  ) |>
  distinct(record_id) |>
  nrow()
interv_manejo_algum_realiz_str <- glue(
  "{interv_manejo_algum_realiz_n}/{interv_manejo_enc_n} ({round(100*interv_manejo_algum_realiz_n/interv_manejo_enc_n, 2)} %)"
)


## Falsos positivos =====================================================
interv_manejo_falsos_positivos_n <- df |>
  filter(redcap_event_name != "Triagem (Arm 1: Participantes)") |>
  mutate(
    falso_positivo = case_when(
      atend_psico_checklist_2 == "Sim" |
        atend_assist_checklist_2 == "Sim" |
        atend_psiq_checklist_2 == "Sim" ~ "Não",
      atend_psico_checklist_2 == "Não" |
        atend_assist_checklist_2 == "Não" |
        atend_psiq_checklist_2 == "Não" ~ "Sim"
    )
  ) |>
  filter(falso_positivo == "Sim") |>
  distinct(record_id) |>
  nrow()
interv_manejo_falsos_positivos_str <- glue::glue(
  "{interv_manejo_falsos_positivos_n}/{interv_manejo_algum_realiz_n} ({round(100*interv_manejo_falsos_positivos_n/interv_manejo_algum_realiz_n, 2)} %)"
)


## Alto Risco ===========================================================
interv_manejo_risco_algum_n <- df |>
  select(
    record_id, redcap_event_name, redcap_repeat_instance,
    atend_psico_checklist_2, atend_psiq_checklist_2, atend_assist_checklist_2
  ) |>
  filter(redcap_event_name != "Triagem (Arm 1: Participantes)") |>
  group_by(record_id, redcap_repeat_instance) |>
  filter(
    atend_psiq_checklist_2 == "Sim" |
      atend_assist_checklist_2 == "Sim" |
      atend_psico_checklist_2 == "Sim"
  ) |>
  distinct(record_id) |>
  nrow()
interv_manejo_risco_algum_str <- glue(
  "{interv_manejo_risco_algum_n}/{interv_manejo_algum_realiz_n} ({round(100*interv_manejo_risco_algum_n/interv_manejo_algum_realiz_n, 2)} %)"
)


## Tabela final =========================================================
interv_manejo_tabela <- tibble(
  `Variável` = c(
    "Encaminhados",
    "Aguardando atendimento",
    # perdas
    "Não retornaram",
    "Desistiram",
    "Ao menos um atendimento realizado",
    "Falsos positivos",
    "Alto risco de suicídio" #,
    # "Motivo do manejo"
  ),
  `Frequência` = c(
    interv_manejo_enc_n,
    interv_manejo_aguard_atend_str,
    interv_manejo_retorno_str,
    interv_manejo_desist_str,
    interv_manejo_algum_realiz_str,
    interv_manejo_falsos_positivos_str,
    interv_manejo_risco_algum_str #,
    # NA_character_
  )
)