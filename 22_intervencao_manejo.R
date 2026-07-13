# Intervenção ==================================================================
## Encaminhados =========================================================
#### Sessões -------------------------------------------------
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

### Geral ----------------------------------------------------
interv_manejo_enc <- df |>
  filter(
    redcap_event_name != "Triagem (Arm 1: Participantes)" &
    (!is.na(atend_psico_dados) | !is.na(atend_psqi_dados) | !is.na(atend_assist_dados))
  ) |>
  select(record_id)
# interv_manejo_enc <- bind_rows(
#   interv_manejo_sa_enc, interv_manejo_s1_enc, interv_manejo_s2_enc, interv_manejo_s3_enc, interv_manejo_s4_enc,
#   interv_manejo_s5_enc, interv_manejo_sf_enc
# )
interv_manejo_enc_ids <- interv_manejo_enc$record_id


interv_manejo_algum_enc_n <- interv_manejo_enc |>
  # pivot_wider(id_cols = "record_id", names_from = "redcap_event_name", values_from = "encaminhado") |>
  # filter(if_any(`Sessao de apresentação (Arm 1: Participantes)`:`Sessao final (Arm 1: Participantes)`, 
  #               ~ . == "Sim")) |>
  # distinct(record_id) |>
  # filter(encaminhado == "Sim") |>
  distinct(record_id) |>
  nrow()

interv_manejo_enc_n <- interv_manejo_enc |>
  # pivot_wider(id_cols = "record_id", names_from = "redcap_event_name", values_from = "encaminhado") |>
  # filter(if_any(`Sessao de apresentação (Arm 1: Participantes)`:`Sessao final (Arm 1: Participantes)`, 
  #               ~ . == "Sim")) |>
  # distinct(record_id) |>
  # filter(encaminhado == "Sim") |>
  nrow()


## Algum atend ===================================================
interv_manejo_algum_realiz_ids <- df |>
  filter(
    redcap_event_name != "Triagem (Arm 1: Participantes)" &
      (if_any(c(atend_psiq_comp_atend_1, atend_psiq_comp_atend_2,
                atend_psiq_comp_atend_3, atend_psiq_comp_atend_4),
              \(x) x == "Sim e realizou atendimento")) |
      (if_any(c(atend_assist_comp_atend_1, atend_assist_comp_atend_2,
                atend_assist_comp_atend_3, atend_assist_comp_atend_4),
              \(x) x == "Sim e realizou atendimento")) |
      (if_any(c(atend_psico_comp_atend_1, atend_psico_comp_atend_2,
                atend_psico_comp_atend_3, atend_psico_comp_atend_4),
              \(x) x == "Sim e realizou atendimento"))
  ) |>
  distinct(record_id) |>
  pull()
interv_manejo_algum_realiz_n <- length(interv_manejo_algum_realiz_ids)

interv_manejo_algum_realiz_str <- glue(
  "{interv_manejo_algum_realiz_n}/{interv_manejo_enc_n} ({round(100*interv_manejo_algum_realiz_n/interv_manejo_enc_n, 2)} %)"
)



## Realizadas ------------------------------------------------
### n Participantes com algum atendimento realizado
# interv_manejo_algum_realiz_ids <- df |>
#   # filter(record_id %in% c(triagem_manejo_eleg_ids, triagem_manejo_nao_eleg_ids)) |>
#   filter(redcap_event_name != 'Triagem (Arm 1: Participantes)') %>% 
#   select(record_id, 
#          enc_sa_superv, enc_sessao_superv,
#          atend_psiq_comp_atend_1, atend_psiq_comp_atend_2, atend_psiq_comp_atend_3, atend_psiq_comp_atend_4,
#          atend_assist_comp_atend_1, atend_assist_comp_atend_2, atend_assist_comp_atend_3, atend_assist_comp_atend_4) %>% 
#   mutate(
#     atend_psicologo = case_when(!is.na(enc_sa_superv) | !is.na(enc_sessao_superv) ~ 'Sim',
#                                 TRUE ~ 'Não'),
#     atend_psiquiatra = case_when((atend_psiq_comp_atend_1 == 'Sim e realizou atendimento' | atend_psiq_comp_atend_2 == 'Sim e realizou atendimento' |
#                                     atend_psiq_comp_atend_3 == 'Sim e realizou atendimento' | atend_psiq_comp_atend_4 == 'Sim e realizou atendimento') ~ 'Sim',
#                                  TRUE ~ 'Não'),
#     atend_assit_social = case_when((atend_assist_comp_atend_1 == 'Sim e realizou atendimento' | atend_assist_comp_atend_2 == 'Sim e realizou atendimento' |
#                                       atend_assist_comp_atend_3 == 'Sim e realizou atendimento' | atend_assist_comp_atend_4 == 'Sim e realizou atendimento') ~ 'Sim',
#                                    TRUE ~ 'Não')
#   ) %>% 
#   select(record_id, atend_psicologo, atend_psiquiatra, atend_assit_social) %>% 
#   filter(atend_psicologo == 'Sim' | atend_psiquiatra == 'Sim' | atend_assit_social == 'Sim') %>% 
#   rename(ID = record_id, `Psicólogo Supervisor` = atend_psicologo, `Psiquiatra` = atend_psiquiatra, `Assistente Social` = atend_assit_social) |>
#   pivot_longer(
#     cols = -ID,
#     names_to = "Especialista",
#     values_to = "atendeu"
#   ) %>% 
#   filter(atendeu == "Sim") |>
#   distinct(ID) |>
#   pull()
# interv_manejo_algum_realiz_n <- length(interv_manejo_algum_realiz_ids)



## Aguardando atendimento ===============================================
interv_manejo_aguard_atend_n <- df |>
  filter(
    (record_id %in% interv_manejo_enc_ids &
     !record_id %in% interv_manejo_algum_realiz_ids) |
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
      (if_any(c(atend_psiq_comp_atend_1, atend_psiq_comp_atend_2,
                atend_psiq_comp_atend_3, atend_psiq_comp_atend_4),
              \(x) x == "Não atendeu")) |
      (if_any(c(atend_assist_comp_atend_1, atend_assist_comp_atend_2,
                atend_assist_comp_atend_3, atend_assist_comp_atend_4),
              \(x) x == "Não atendeu")) |
      (if_any(c(atend_psico_comp_atend_1, atend_psico_comp_atend_2,
                atend_psico_comp_atend_3, atend_psico_comp_atend_4),
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
  filter(record_id %in% interv_manejo_algum_realiz_ids) |>
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



## Não Alto Risco =====================================================
interv_manejo_risco_nao_n <- interv_manejo_algum_realiz_n - interv_manejo_risco_algum_n



## Elegíveis --------------------------------------------------
interv_manejo_eleg_ids <- df |>
  filter(
    record_id %in% interv_manejo_algum_realiz_ids,
    redcap_event_name != 'Triagem (Arm 1: Participantes)'
  ) %>% 
  filter(
    enc_sa_superv_apto %in% "1 - Sim" |
      enc_sessao_superv_apto %in% "1 - Sim"
  ) |>
  distinct(record_id) |>
  pull()
interv_manejo_eleg_n <- length(interv_manejo_eleg_ids)


## Não elegíveis  ----------------------------------------------
interv_manejo_nao_eleg_ids <- df |>
  filter(
    record_id %in% interv_manejo_algum_realiz_ids,
    redcap_event_name != 'Triagem (Arm 1: Participantes)'
  ) %>% 
  filter(
    enc_sa_superv_apto %in% "2 - Não" |
      enc_sessao_superv_apto %in% "2 - Não"
  ) |>
  distinct(record_id) |>
  pull()
interv_manejo_nao_eleg_n <- length(interv_manejo_nao_eleg_ids)


## Motivo encaminhamento ---------------------------------------
interv_manejo_motivo <- df |>
  filter(
    redcap_event_name != "Triagem (Arm 1: Participantes)" &
      record_id %in% interv_manejo_algum_realiz_ids
  ) |>
  group_by(record_id, redcap_event_name) |>
  filter(
    if_any(c(enc_sa_superv, enc_sa_atend_ind, enc_sessao_superv, enc_sessao_atend_ind),
           \(x) x == "Sim")
  ) |>
  summarise(
    Motivo = case_when(
      phq9_perg_9 != "Nenhuma vez" &
        !is.na(phq9_perg_9) ~ "PHQ-9",
      TRUE              ~ "Outro motivo"
    )
  ) |>
  ungroup() |>
  with(rstatix::freq_table(Motivo)) |>
  arrange(group)

interv_manejo_motivo_str <- interv_manejo_motivo |>
  mutate(linha = glue("{group} = {n}")) |>
  pull(linha) |>
  paste(collapse = "\n")




## Tipo do encaminhamento ================================================
interv_manejo_tipo_df <- df |>
  filter(redcap_event_name != "Triagem (Arm 1: Participantes)") |>
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
    ID = record_id,
    `Psicólogo Supervisor` = atend_psi, 
    `Psiquiatra`           = atend_psiq, 
    `Assistente Social`    = atend_assist
  )

interv_manejo_tipo_tabela <- interv_manejo_tipo_df |>
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


interv_manejo_tipo_str <- interv_manejo_tipo_tabela |>
  arrange(`n (%)`) |>
  mutate(linha = glue("{Especialista} = {`n (%)`}")) |>
  pull(linha) |>
  paste(collapse = "\n")



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
