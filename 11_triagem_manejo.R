# Motivo de manejo: atend_esp_motivo_encam, atend_esp_motivo_encam_2, 
# atend_esp_motivo_encam_3, atend_esp_motivo_encam_4
# Perdas: 
## Falta de retorno: 
## if_all(c(atend_assist_comp_atend_1, atend_assist_comp_atend_2,
## atend_assist_comp_atend_3, atend_assist_comp_atend_4, ... ),
## ~ is.na(.) | . == "Não atendeu")
## Desistência
## 
## Encaminhamentos --------------------------------------------
# n de participantes encaminhados para manejo
# Fizeram Manejo ==============================================================
# foram encaminhados para manejo
tri_manejo_enc_ids <- df %>% 
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  filter(fez_enc_manejo_superv == "Sim") |> 
  distinct(record_id) |>
  pull()
tri_manejo_enc_n <- length(tri_manejo_enc_ids)

# ## Aguardando atendimento ------------------------------------
# tri_ids_aguardando_atend <- df |>
#   filter(
#     # Form Encerramento - Triagem
#     (particip_eleg_continuidade %in% "Aguardando avaliação do psiquiatra" |
#        particip_eleg_continuidade %in% "Aguardando avaliação do supervisor" |
#        particip_eleg_continuidade_2 %in% "Aguardando avaliação do psiquiatra" |
#        particip_eleg_continuidade_3 %in% "Aguardando avaliação do psiquiatra") |
#       # Form Atendimento - Psiquiatra
#       (atend_psiq_comp_atend_3 %in% "Sim, mas reagendou" & is.na(atend_psiq_comp_atend_4)) |
#       (atend_psiq_comp_atend_2 %in% "Sim, mas reagendou" & is.na(atend_psiq_comp_atend_3)) |
#       (atend_psiq_comp_atend_1 %in% "Sim, mas reagendou" & is.na(atend_psiq_comp_atend_2)) |
#       # Form Atendimento - Assistente Social
#       (atend_assist_comp_atend_3 %in% "Sim, mas reagendou" & is.na(atend_assist_comp_atend_4)) |
#       (atend_assist_comp_atend_2 %in% "Sim, mas reagendou" & is.na(atend_assist_comp_atend_3)) |
#       (atend_assist_comp_atend_1 %in% "Sim, mas reagendou" & is.na(atend_assist_comp_atend_2))
#   ) |>
#   distinct(record_id) |>
#   pull()
# 
# tri_manejo_aguardando_atend_n <- df |>
#   filter(record_id %in% tri_ids_aguardando_atend) |>
#   nrow()
# tri_manejo_aguardando_atend_n <- glue::glue(
#   "{tri_manejo_aguardando_atend_n}/{tri_manejo_enc_n} ({round(100*tri_manejo_aguardando_atend_n/tri_manejo_enc_n, 2)} %)"
# )
# 
# 
# ## Aguardando atendimento ====================================
# tabela_aguardando_atendimento <- df |>
#   filter(redcap_event_name == "Triagem (Arm 1: Participantes)") |>
#   reframe(
#     aguardando = case_when(
#       particip_eleg_continuidade == "Aguardando avaliação do psiquiatra" |
#         particip_eleg_continuidade_1 == "Aguardando avaliação do psiquiatra" |
#         particip_eleg_continuidade == "Aguardando avaliação do supervisor" |
#         particip_eleg_continuidade_2 == "Aguardando avaliação do psiquiatra" |
#         particip_eleg_continuidade_3 == "Aguardando avaliação do psiquiatra" ~ "Sim",
#       TRUE ~ "Não"
#     )
#   ) |> 
#   with(rstatix::freq_table(aguardando))
# tri_manejo_aguardando_atend_n <- glue::glue(
#   "{tabela_aguardando_atendimento$n[2]}/{tri_manejo_enc_n} ({round(100*tabela_aguardando_atendimento$n[2]/tri_manejo_enc_n, 2)} %)"
#   # "{tabela_aguardando_atendimento$n[2]} ({round(100*tabela_aguardando_atendimento$n[2]/n_tri_encam, 2)} %)"
# )





## Realizadas ------------------------------------------------
### n Participantes com algum atendimento realizado
# n_tri_manejo_realiz <- glue("{tri_manejo_nao_eleg + tri_manejo_eleg}")
tri_manejo_algum_realiz_ids <- df |>
  # filter(record_id %in% c(triagem_manejo_eleg_ids, triagem_manejo_nao_eleg_ids)) |>
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  select(record_id, 
         ev_manejo_superv,
         atend_psiq_comp_atend_1, atend_psiq_comp_atend_2, atend_psiq_comp_atend_3, atend_psiq_comp_atend_4,
         atend_assist_comp_atend_1, atend_assist_comp_atend_2, atend_assist_comp_atend_3, atend_assist_comp_atend_4) %>% 
  mutate(
    atend_psicologo = case_when(!is.na(ev_manejo_superv) ~ 'Sim',
                                TRUE ~ 'Não'),
    atend_psiquiatra = case_when((atend_psiq_comp_atend_1 == 'Sim e realizou atendimento' | atend_psiq_comp_atend_2 == 'Sim e realizou atendimento' |
                                    atend_psiq_comp_atend_3 == 'Sim e realizou atendimento' | atend_psiq_comp_atend_4 == 'Sim e realizou atendimento') ~ 'Sim',
                                 TRUE ~ 'Não'),
    atend_assit_social = case_when((atend_assist_comp_atend_1 == 'Sim e realizou atendimento' | atend_assist_comp_atend_2 == 'Sim e realizou atendimento' |
                                      atend_assist_comp_atend_3 == 'Sim e realizou atendimento' | atend_assist_comp_atend_4 == 'Sim e realizou atendimento') ~ 'Sim',
                                   TRUE ~ 'Não')
  ) %>% 
  select(record_id, atend_psicologo, atend_psiquiatra, atend_assit_social) %>% 
  filter(atend_psicologo == 'Sim' | atend_psiquiatra == 'Sim' | atend_assit_social == 'Sim') %>% 
  rename(ID = record_id, `Psicólogo Supervisor` = atend_psicologo, `Psiquiatra` = atend_psiquiatra, `Assistente Social` = atend_assit_social) |>
  pivot_longer(
    cols = -ID,
    names_to = "Especialista",
    values_to = "atendeu"
  ) %>% 
  filter(atendeu == "Sim") |>
  distinct(ID) |>
  pull()

tri_manejo_algum_realiz_n <- length(tri_manejo_algum_realiz_ids)


# tri_manejo_algum_realiz_n <- df |>
#   filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
#   filter(
#     !is.na(particip_eleg_continuidade_3) |
#       !is.na(particip_eleg_continuidade_2) |
#       !is.na(particip_eleg_continuidade)
#   ) |>
#   nrow()
# df |>
# select(
#   record_id,
#   atend_psiq_checklist_1, atend_assist_checklist_1
# ) |>
# filter(
#   atend_psiq_checklist_1 == "Sim" |
#     atend_assist_checklist_1 == "Sim"
#   ) |>
# distinct(record_id) |>
# nrow()
# tri_manejo_algum_realiz_n <- glue::glue("{tri_manejo_algum_realiz_n}/{tri_manejo_enc_n} ({round(100*tri_manejo_algum_realiz_n/tri_manejo_enc_n, 2)} %)")

tri_manejo_realiz_n <- df |>
  select(
    record_id, redcap_event_name, redcap_repeat_instance,
    atend_psiq_checklist_1, atend_assist_checklist_1
  ) |>
  # filter(redcap_event_name == "Triagem (Arm 1: Participantes)") |>
  summarise(
    realizados = sum(atend_psiq_checklist_1 == "Sim", na.rm = TRUE) + 
      sum(atend_assist_checklist_1 == "Sim", na.rm = TRUE)
  ) |>
  pull()



## Perdas --------------------------------------------------
### Desistência -------------------------------------------
#### checa se nao compareceu, basicamente
# df |> filter(atend_psiq_checklist_1 == "Não") |> select(atend_psiq_checklist_1_1)
tri_manejo_desist_ids <- df |>
  filter(
    # (fez_enc_manejo_superv == "Sim" & is.na(ev_manejo_superv)) |
    (atend_psiq_checklist_1 == "Não" | atend_assist_checklist_1 == "Não")
  ) |>
  distinct(record_id) |>
  pull()

tri_manejo_desist_n <- length(tri_manejo_desist_ids)

tri_manejo_desist_n <- glue(
  "{tri_manejo_desist_n}/{tri_manejo_enc_n} ({round(100*tri_manejo_desist_n/tri_manejo_enc_n, 2)} %)"
)

### Falta de retorno --------------------------------------
tri_manejo_retorno_ids <- df |>
  filter(
    redcap_event_name == "Triagem (Arm 1: Participantes)" &
      # (atend_psiq_comp_atend_1 != "Sim e realizou atendimento" &
      # atend_psiq_comp_atend_2 != "Sim e realizou atendimento" &
      # atend_psiq_comp_atend_3 != "Sim e realizou atendimento" &
      # atend_psiq_comp_atend_4 != "Sim e realizou atendimento") |
      (if_any(c(atend_psiq_comp_atend_1,atend_psiq_comp_atend_2,
                atend_psiq_comp_atend_3,atend_psiq_comp_atend_4),
              \(x) x %in% "Não atendeu") |
         # (atend_assist_comp_atend_1 != "Sim e realizou atendimento" &
         #    atend_assist_comp_atend_2 != "Sim e realizou atendimento" &
         #    atend_assist_comp_atend_3 != "Sim e realizou atendimento" &
         #    atend_assist_comp_atend_4 != "Sim e realizou atendimento")
         if_any(c(atend_assist_comp_atend_1,atend_assist_comp_atend_2,
                  atend_assist_comp_atend_3,atend_assist_comp_atend_4),
                \(x) x %in% "Não atendeu")),
    # TO-DO: adicionar falta de retorno do atendimento com psicólogo
    # via form Desfecho
  ) |>
  distinct(record_id) |>
  pull()

tri_manejo_retorno_n <- length(tri_manejo_retorno_ids)

tri_manejo_retorno_str <- glue(
  "{tri_manejo_retorno_n}/{tri_manejo_enc_n} ({round(100*tri_manejo_retorno_n/tri_manejo_enc_n, 2)} %)"
)



## Aguardando atendimento ----------------------------------
tri_manejo_aguardando_atend_ids <- df |>
  filter(
    redcap_event_name == "Triagem (Arm 1: Participantes)" &
      aceita_tcle %in% "Aceito participar do estudo" &
      fez_enc_manejo_superv %in% "Sim" &
      # !is.na(gad7_perg_1) &
      !record_id %in% c(
        tri_manejo_desist_ids, 
        tri_manejo_algum_realiz_ids,
        tri_manejo_retorno_ids, trim_manejo_eleg_ids, tri_manejo_nao_eleg_ids,
        interv_sa_realiz_ids
      )
  ) |>
  distinct(record_id) |>
  pull()

tri_manejo_aguardando_atend_n <- length(tri_manejo_aguardando_atend_ids)


## Alto Risco ----------------------------------------------
tri_manejo_risco_algum_ids <- df |>
  select(
    record_id, redcap_event_name, redcap_repeat_instance,
    atend_psiq_checklist_2, atend_assist_checklist_2
  ) |>
  filter(redcap_event_name == "Triagem (Arm 1: Participantes)") |>
  filter(
    atend_psiq_checklist_2 == "Sim" |
      atend_assist_checklist_2 == "Sim"
  ) |>
  distinct(record_id) |>
  pull()

tri_manejo_risco_algum_n <- length(tri_manejo_risco_algum_ids)
# tri_manejo_risco_algum_n <- glue(
#   "{tri_manejo_risco_algum_n}/{as.numeric(substr(tri_manejo_algum_realiz_n,1,2))} ({round(100*tri_manejo_risco_algum_n/as.numeric(substr(tri_manejo_algum_realiz_n,1,2)), 2)} %)"
# )
tri_manejo_risco_algum_str <- glue(
  "{tri_manejo_risco_algum_n}/{as.numeric(tri_manejo_algum_realiz_n)} ({round(100*tri_manejo_risco_algum_n/as.numeric(tri_manejo_algum_realiz_n), 2)} %)"
)


tri_n_risco <- df |>
  summarise(
    risco_suic = sum(c(atend_psico_checklist_2 == "Sim"), na.rm = TRUE)
  ) |>
  pull()
tri_n_risco <- glue(
  "{tri_n_risco}/{tri_manejo_enc_n} ({round(100*tri_n_risco/tri_manejo_enc_n, 2)} %)"
  # "{tri_n_risco} ({round(100*tri_n_risco/tri_manejo_algum_realiz_n, 2)} %)"
)



## Não Alto Risco ==============================================





## Falsos positivos ==========================================
tri_manejo_falsos_positivos_ids <- df |>
  filter(redcap_event_name == "Triagem (Arm 1: Participantes)") |>
  reframe(
    record_id = record_id,
    falso_positivo = case_when(
      particip_eleg_continuidade == "Não" |
        particip_eleg_continuidade_2 == "Não" |
        particip_eleg_continuidade_3 == "Não" ~ "Não",
      particip_eleg_continuidade == "Sim" |
        particip_eleg_continuidade_2 == "Sim" |
        particip_eleg_continuidade_3 == "Sim" ~ "Sim",
      TRUE                                    ~ NA
    )
  ) |>
  filter(falso_positivo == "Sim") |>
  distinct(record_id) |>
  pull()

tri_manejo_falsos_positivos_tabela <- df |>
  filter(redcap_event_name == "Triagem (Arm 1: Participantes)") |>
  reframe(
    falso_positivo = case_when(
      particip_eleg_continuidade == "Não" |
        particip_eleg_continuidade_2 == "Não" |
        particip_eleg_continuidade_3 == "Não" ~ "Não",
      particip_eleg_continuidade == "Sim" |
        particip_eleg_continuidade_2 == "Sim" |
        particip_eleg_continuidade_3 == "Sim" ~ "Sim",
      TRUE                                    ~ NA
    )
  ) |> 
  with(rstatix::freq_table(falso_positivo))
tri_manejo_falsos_positivos_str <- glue::glue(
  "{tri_manejo_falsos_positivos_tabela$n[2]}/{tri_manejo_falsos_positivos_tabela$n[1]+tri_manejo_falsos_positivos_tabela$n[2]} ({tri_manejo_falsos_positivos_tabela$prop[2]} %)"
  # "{tri_manejo_falsos_positivos_tabela$n[2]} ({tri_manejo_falsos_positivos_tabela$prop[2]} %)"
)



## Motivo encaminhamento ---------------------------------------
tri_manejo_motivo <- df |>
  filter(
    redcap_event_name == "Triagem (Arm 1: Participantes)" &
      record_id %in% tri_manejo_algum_realiz_ids
  ) |>
  group_by(record_id) |>
  summarise(
    Motivo = case_when(
      score_phq_9 >= 10 ~ "PHQ-9 ≥ 10",
      TRUE              ~ "Outro motivo"
    )
  ) |>
  ungroup() |>
  with(rstatix::freq_table(Motivo)) |>
  arrange(group)
# filter(redcap_event_name == "Triagem (Arm 1: Participantes)") |>
# summarise(
#   atend_esp_motivo_encam_mental = sum(
#     c(atend_esp_motivo_encam___0   == "Checked" & agend_esp_momento_obs == "Avaliação inicial",
#       atend_esp_motivo_encam_2___0 == "Checked",
#       atend_esp_motivo_encam_3___0 == "Checked",
#       atend_esp_motivo_encam_4___0 == "Checked"),
#     na.rm = TRUE
#   ),
#   atend_esp_motivo_encam_social = sum(
#     c(atend_esp_motivo_encam___1   == "Checked" & agend_esp_momento_obs == "Avaliação inicial",
#       atend_esp_motivo_encam_2___1 == "Checked",
#       atend_esp_motivo_encam_3___1 == "Checked",
#       atend_esp_motivo_encam_4___1 == "Checked"),
#     na.rm = TRUE
#   ),
#   atend_esp_motivo_encam_outro = sum(
#     c(atend_esp_motivo_encam___2   == "Checked" & agend_esp_momento_obs == "Avaliação inicial",
#       atend_esp_motivo_encam_2___2 == "Checked",
#       atend_esp_motivo_encam_3___2 == "Checked",
#       atend_esp_motivo_encam_4___2 == "Checked"),
#     na.rm = TRUE
#   )
# )
tri_manejo_motivo_str <- tri_manejo_motivo |>
  mutate(linha = glue("{group} = {n}")) |>
  pull(linha) |>
  paste(collapse = "\n")





## Tipo de atendimento =========================================================
tri_manejo_tipo_tabela <- df %>% 
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  select(record_id, ev_manejo_superv, 
         atend_psiq_comp_atend_1, atend_psiq_comp_atend_2, atend_psiq_comp_atend_3, atend_psiq_comp_atend_4,
         atend_assist_comp_atend_1, atend_assist_comp_atend_2, atend_assist_comp_atend_3, atend_assist_comp_atend_4) %>% 
  mutate(
    atend_psicologo = case_when(!is.na(ev_manejo_superv) ~ 'Sim',
                                TRUE ~ 'Não'),
    atend_psiquiatra = case_when((atend_psiq_comp_atend_1 == 'Sim e realizou atendimento' | atend_psiq_comp_atend_2 == 'Sim e realizou atendimento' |
                                    atend_psiq_comp_atend_3 == 'Sim e realizou atendimento' | atend_psiq_comp_atend_4 == 'Sim e realizou atendimento') ~ 'Sim',
                                 TRUE ~ 'Não'),
    atend_assit_social = case_when((atend_assist_comp_atend_1 == 'Sim e realizou atendimento' | atend_assist_comp_atend_2 == 'Sim e realizou atendimento' |
                                      atend_assist_comp_atend_3 == 'Sim e realizou atendimento' | atend_assist_comp_atend_4 == 'Sim e realizou atendimento') ~ 'Sim',
                                   TRUE ~ 'Não')
  ) %>% 
  select(record_id, atend_psicologo, atend_psiquiatra, atend_assit_social) %>% 
  filter(atend_psicologo == 'Sim' | atend_psiquiatra == 'Sim' | atend_assit_social == 'Sim') %>% 
  rename(ID = record_id, `Psicólogo Supervisor` = atend_psicologo, `Psiquiatra` = atend_psiquiatra, `Assistente Social` = atend_assit_social) |>
  pivot_longer(
    cols = -ID,
    names_to = "Especialista",
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

tri_manejo_psi_n <- tri_manejo_tipo_tabela |> 
  filter(Especialista == "Psicólogo Supervisor") |>
  pull(`n (%)`)

tri_manejo_psiq_n <- tri_manejo_tipo_tabela |> 
  filter(Especialista == "Psiquiatra") |>
  pull(`n (%)`)

tri_manejo_assist_n <- tri_manejo_tipo_tabela |> 
  filter(Especialista == "Assistente Social") |>
  pull(`n (%)`)

tri_manejo_tipo_str <- tri_manejo_tipo_tabela |>
  arrange(`n (%)`) |>
  mutate(linha = glue("{Especialista} = {`n (%)`}")) |>
  pull(linha) |>
  paste(collapse = "\n")


## Tabela final ================================================================
tri_manejo_tabela <- tibble(
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
    tri_manejo_enc_n,
    tri_manejo_aguardando_atend_n,
    tri_manejo_retorno_str,
    tri_manejo_desist_n,
    tri_manejo_algum_realiz_n,
    tri_manejo_falsos_positivos_str,
    tri_manejo_risco_algum_str #,
    # NA_character_
  )
)

tri_manejo_kable <- tri_manejo_tabela %>%
  kable(
    format   = "html", 
    booktabs = TRUE, 
    align    = c("l", "c"), # alinha primeira linha a esquerda e segunda no centro
    caption  = "Resumo de atendimento especializado na Triagem"
  ) %>%
  kable_styling(
    bootstrap_options = c("hover", "condensed"), 
    full_width        = FALSE, 
    position          = "center",
    html_font         = "Arial"
  ) %>%
  # row_spec(0, bold = TRUE) %>% 
  row_spec(0, bold = TRUE, color = "black", extra_css = "border-top: 1.5px solid black; border-bottom: 1px solid black;") %>%
  row_spec(4, extra_css = "border-bottom: 1px solid black;") %>% # Adiciona linha acima da linha 5
  pack_rows("Perdas", 3, 4, bold = FALSE, label_row_css = "border-bottom: none;") # Cria o grupo "Perdas"
