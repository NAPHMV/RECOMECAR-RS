# Dados andamento ===========================================================
# ids que aceitaram TCLE na Sessão de Apresentação
interv_ids <- df %>% 
  filter(redcap_event_name == 'Sessao de apresentação (Arm 1: Participantes)') %>% 
  select(record_id, tcle_aceite) %>% 
  filter(tcle_aceite == 'Sim') %>% 
  select(record_id) %>% 
  pull()


# Monta banco indicadores intervenção
aux_interv_sessoes <- list(
  list(event = "Sessao 1 (Arm 1: Participantes)",     prefixo = "sessao_1"),
  list(event = "Sessao 2 (Arm 1: Participantes)",     prefixo = "sessao_2"),
  list(event = "Sessao 3 (Arm 1: Participantes)",     prefixo = "sessao_3"),
  list(event = "Sessao 4 (Arm 1: Participantes)",     prefixo = "sessao_4"),
  list(event = "Sessao 5 (Arm 1: Participantes)",     prefixo = "sessao_5"),
  list(event = "Sessao final (Arm 1: Participantes)", prefixo = "sessao_final")
)

### base sessão de apresentação
aux_interv_base <- df |>
  filter(record_id %in% interv_ids,
         redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)") |>
  select(record_id, enc_sa_agend_data, 
         dta_sessao_a, data_reagend_sa_1, data_reagend_sa_2, data_reagend_sa_3,
         desist_motivo_reagend_sa_1) |>
  mutate(
    across(c(data_reagend_sa_1, data_reagend_sa_2, data_reagend_sa_3), \(x) as.character(as.Date(x))),
    
    sessao_A_realizada = as.integer(!is.na(enc_sa_agend_data)),
    motivo_desistencia_sessao_A = desist_motivo_reagend_sa_1,
    desistencia_sessao_A = as.integer(!is.na(motivo_desistencia_sessao_A))
  ) |>
  group_by(record_id) |>
  mutate(
    sessao_A_data  = max(c(dta_sessao_a, data_reagend_sa_1, data_reagend_sa_2, data_reagend_sa_3), na.rm = TRUE),
  ) |>
  ungroup() |>
  select(-c(enc_sa_agend_data, dta_sessao_a, data_reagend_sa_1, data_reagend_sa_2, data_reagend_sa_3))

### base das demais sessões
interv_andamento_df <- aux_interv_sessoes |>
  purrr::map(\(s) processa_sessao(df, interv_ids, s$event, s$prefixo)) |>
  ### une bases
  purrr::reduce(left_join, by = "record_id", .init = aux_interv_base)

# ajusta Sessão Final realizada
interv_andamento_df <- interv_andamento_df |>
  left_join(
    df |>
      filter(
        redcap_event_name == "Desfecho (Arm 1: Participantes)",
        desfecho_participante_interv == "Completou a participação") |>
      mutate(
        sessao_final_realizada = 1
      ) |>
      select(record_id, sessao_final_realizada),
    by = c('record_id', 'sessao_final_realizada')
  )



# Sessão A =====================================================================
## Aguardando convite -----------------------------------------
interv_sa_aguard_convite_ids <- df |>
  # nao abriu entrada na Sessão A
  group_by(record_id) |>
  filter(!"Sessao de apresentação (Arm 1: Participantes)" %in% redcap_event_name) |>
  ungroup() |>
  # é elegível para intervenção
  filter(
    record_id %in% tri_eleg_interv_ids
    # particip_eleg_continuidade == "Sim"
  ) |>
  distinct(record_id) |>
  # junta com quem abriu enrtada na Sessão A, mas não tem tentativa de contato
  full_join(
    df |>
      filter(
        redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)" &
          is.na(tentativa_contato_realiz_1)
      ) |>
      distinct(record_id),
    by = "record_id"
  ) |>
  pull()

interv_sa_aguard_convite_n <- length(interv_sa_aguard_convite_ids)
  

## Aguardando agendamento -------------------------------------
interv_aguard_agend_cols <- grep("^tentativa_agendar_sessao_", names(df), value = TRUE)
# Ordena garantindo ordem numérica: _1, _2, _3...
interv_aguard_agend_cols <- interv_aguard_agend_cols[
  order(as.integer(stringr::str_extract(interv_aguard_agend_cols, "\\d+$")))]

interv_sa_aguard_agend_ids <- df |>
  filter(
    redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)",
    record_id %in% tri_eleg_interv_ids,
    if_any(tentativa_contato_realiz_1:tentativa_contato_realiz_6,  \(x) x %in% "Sim"),
    !if_any(tentativa_agendar_sessao_1:tentativa_agendar_sessao_6, \(x) x %in% "Sim")
  ) |>
  rowwise() |>
  mutate(
    # ultima_tentativa_col = {
    #   vals <- c_across(all_of(interv_aguard_agend_cols))
    #   idx  <- max(which(vals %in% c("Não")), na.rm = TRUE)  # último "Não" ou NA
    #   if (is.finite(idx)) interv_aguard_agend_cols[idx] else NA_character_
    # },
    ultima_tentativa_val = {
      vals <- c_across(all_of(interv_aguard_agend_cols))
      idx  <- max(which(vals %in% c("Não")), na.rm = TRUE)  # último "Não" ou NA
      if (is.finite(idx)) vals[idx] else NA_character_
    }
  ) |>
  ungroup() |>
  select(record_id, ultima_tentativa_val) |>
  distinct(record_id) |>
  pull()

interv_sa_aguard_agend_n <- length(interv_sa_aguard_agend_ids)



## Finalizadas -----------------------------------------------
interv_sa_realiz_ids <- interv_andamento_df |> 
  filter(sessao_A_realizada == 1) |> 
  distinct(record_id) |>
  pull()

# interv_sa_realiz_ids <- df |>
#   filter(
#     redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)",
#     !is.na(enc_sa_agend_data)
#   ) |>
#   distinct(record_id) |>
#   pull()

interv_sa_realiz_n <- length(interv_sa_realiz_ids)


## Exclusão -------------------------------------------------
# interv_exclusao_motivo <- df |>
#   with(table(enc_sessao_motivo)) |>
#   as.data.frame() |>
#   rename(motivo = enc_sessao_motivo, n = Freq) |>
#   bind_rows(
#     df |>
#       filter(enc_sessao_superv_apto == "2 - Não") |>
#       distinct(record_id) %>%
#       reframe(motivo = "Não elegível após atendimento individual", n = nrow(.))
#   )
# 
# interv_exclusao_str <- interv_exclusao_motivo |>
#   filter(n > 0) |>
#   mutate(linha = glue("{motivo} = {n}")) |>
#   pull(linha) |>
#   paste(collapse = "\n")

# interv_sa_exclusao_ids <- df |>
#   # group_by(redcap_event_name) |>
#   filter(
#     enc_sa_agend_data == "Não" |
#       enc_sa_superv_apto == "2 - Não"
#   ) |>
#   distinct(record_id) |>
#   pull()
# 
# interv_sa_exclusao_n <- length(interv_sa_exclusao_ids)
interv_sa_desist_ids <- df |>
  filter(
    redcap_event_name == "Desfecho (Arm 1: Participantes)",
    desfecho_participante_interv == "Retirado",
    desfecho_participante_motivo_interv == "Desistência"
  ) |>
  distinct(record_id) |>
  pull()
interv_sa_desist_n <- length(interv_sa_desist_ids)

# interv_sa_exclusao_ids <- df |>
#   filter(
#     !record_id %in% (interv_andamento_df |> 
#       filter(sessao_1_realizada == 1) |>
#       distinct(record_id) |>
#       pull()),
#     redcap_event_name == "Desfecho (Arm 1: Participantes)",
#     desfecho_participante_interv == "Retirado",
#     desfecho_participante_motivo_interv == "Critério de exclusão"
#   ) |>
#   distinct(record_id) |>
#   pull()
interv_sa_exclusao_ids <- df |>
  filter(
    record_id %in% interv_sa_realiz_ids &
      !record_id %in% (
        interv_andamento_df |> 
                filter(sessao_1_realizada == 1) |>
                distinct(record_id) |>
                pull()
      )
    ) |>
  filter(
    redcap_event_name == "Desfecho (Arm 1: Participantes)",
    desfecho_participante_interv == "Retirado"#,
    # desfecho_participante_motivo_interv == "Critério de exclusão"
  ) |>
  distinct(record_id) |>
  pull()
interv_sa_exclusao_n <- length(interv_sa_exclusao_ids)

interv_sa_perda_ids <- df |>
  filter(
    !record_id %in% (interv_andamento_df |> 
                       filter(sessao_1_realizada == 1) |>
                       distinct(record_id) |>
                       pull()),
    redcap_event_name == "Desfecho (Arm 1: Participantes)",
    desfecho_participante_interv == "Retirado",
    desfecho_participante_motivo_interv == "Perda de acompanhamento"
  ) |>
  distinct(record_id) |>
  pull()

interv_sa_exclusao_str <- df |>
  filter(record_id %in% interv_sa_realiz_ids) |>
  anti_join(
    df |>
      filter(redcap_event_name == "Sessao 1 (Arm 1: Participantes)"  &
               !is.na(gad7_perg_1)) |>
      select(record_id),
    by = "record_id"
  ) |>
  filter(
    redcap_event_name == "Desfecho (Arm 1: Participantes)",
    desfecho_participante_interv == "Retirado"
  ) |>
  select(
    # desfecho_participante_interv, 
    desfecho_participante_motivo_interv #, 
         # desfecho_participante_motivo_exclu_interv___1:desfecho_participante_motivo_exclu_interv___5
  ) |>
  with(rstatix::freq_table(desfecho_participante_motivo_interv)) |>
  arrange(group) |>
  mutate(linha = glue("{group} = {n}")) |>
  pull(linha) |>
  paste(collapse = "\n")
  



## Não iniciam -------------------------------------------
interv_sa_nao_concordam_ids <- df |>
  filter(
    redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)",
    !is.na(desist_motivo_sa)
  ) |>
  distinct(record_id) |>
  pull()

interv_sa_nao_concordam_n <- length(interv_sa_nao_concordam_ids)

interv_sa_naoinicia_motivo <- df |>
  with(table(desist_motivo_sa)) |>
  as.data.frame() |>
  rename(motivo = desist_motivo_sa, n1 = Freq) |>
  bind_rows(
    df |>
      filter(nao_comp_motivo_sa == "Óbito") |>
      distinct(record_id) %>%
      reframe(motivo = "Óbito", n1 = nrow(.))
  ) |>
  left_join(
    df |>
      with(table(desist_motivo_reagend_sa_1)) |>
      as.data.frame() |>
      rename(motivo = desist_motivo_reagend_sa_1, n2 = Freq)
  ) |>
  rows_update(
    df |>
      filter(nao_comp_motivo_reagend_sa_1 == "Óbito") |>
      distinct(record_id) %>%
      reframe(motivo = "Óbito", n2 = nrow(.)),
    by = 'motivo'
  ) |>
  left_join(
    df |>
      with(table(desist_motivo_reagend_sa_2)) |>
      as.data.frame() |>
      rename(motivo = desist_motivo_reagend_sa_2, n3 = Freq)
  ) |>
  rows_update(
    df |>
      filter(nao_comp_motivo_reagend_sa_2 == "Óbito") |>
      distinct(record_id) %>%
      reframe(motivo = "Óbito", n3 = nrow(.)),
    by = 'motivo'
  ) |>
  left_join(
    df |>
      with(table(desist_motivo_reagend_sa_3)) |>
      as.data.frame() |>
      rename(motivo = desist_motivo_reagend_sa_3, n4 = Freq)
  ) |>
  rows_update(
    df |>
      filter(nao_comp_motivo_reagend_sa_3_2 == "Óbito") |>
      distinct(record_id) %>%
      reframe(motivo = "Óbito", n4 = nrow(.)),
    by = 'motivo'
  ) |>
  group_by(motivo) |>
  mutate(
    n = sum(c(n1, n2, n3, n4))
  ) |>
  select(motivo, n) |>
  ungroup()

interv_sa_naoinicia_n <- sum(interv_sa_naoinicia_motivo$n)

interv_sa_naoinicia_str <- interv_sa_naoinicia_motivo |>
  arrange(-n) |>
  # filter(n > 0) |>
  mutate(linha = glue("{motivo} = {n}")) |>
  pull(linha) |>
  paste(collapse = "\n")



## Aguardando sessão ------------------------------------------
interv_sa_aguardando_cols <- grep("^tentativa_agendar_sessao_", names(df), value = TRUE)
interv_sa_aguardando_cols <- interv_sa_aguardando_cols[
  order(as.integer(stringr::str_extract(interv_sa_aguardando_cols, "\\d+$")))]

interv_sa_aguardando_ids <- df |>
  filter(
    redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)",
    !record_id %in% interv_sa_realiz_ids,
    if_any(tentativa_contato_realiz_1:tentativa_contato_realiz_6,  \(x) x %in% "Sim"),
    if_any(tentativa_agendar_sessao_1:tentativa_agendar_sessao_6, \(x) x %in% "Sim")
  ) |>
  rowwise() |>
  mutate(
    ultima_tentativa_val = {
      vals <- c_across(all_of(interv_sa_aguardando_cols))
      idx  <- max(which(!is.na(vals)), na.rm = TRUE)  # índice do último não-NA
      if (is.finite(idx)) vals[idx] else NA_character_
    }
  ) |>
  ungroup() |>
  select(record_id, ultima_tentativa_val) |>
  distinct(record_id) |>
  pull()

interv_sa_aguardando_n <- length(interv_sa_aguardando_ids)



## PSYCHLOPS -----------------------------------------------
df |>
  filter(redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)") |>
  summarise(
    psychlops_media = mean(psychlops_score_1, na.rm = TRUE)
  ) |>
  pull(psychlops_media)



# Sessão 1 =====================================================================
## Aguardando agendamento --------------------------------------
interv_s1_aguard_agend_ids <- interv_aguardando_agendamento(sessao = 1)
interv_s1_aguard_agend_n <- length(interv_s1_aguard_agend_ids)

## Aguardando sessão -------------------------------------------
s1_aguardando_ids <- interv_aguardando_sessao(sessao = 1)
s1_aguardando_n <- length(s1_aguardando_ids)


# Sessão 2 =====================================================================
## Aguardando agendamento --------------------------------------
interv_s2_aguard_agend_ids <- interv_aguardando_agendamento(sessao = 2)
interv_s2_aguard_agend_n <- length(interv_s2_aguard_agend_ids)

## Aguardando sessão -------------------------------------------
s2_aguardando_ids <- interv_aguardando_sessao(sessao = 2)
s2_aguardando_n <- length(s2_aguardando_ids)


# Sessão 3 =====================================================================
## Aguardando agendamento --------------------------------------
interv_s3_aguard_agend_ids <- interv_aguardando_agendamento(sessao = 3)
interv_s3_aguard_agend_n <- length(interv_s3_aguard_agend_ids)

## Aguardando sessão -------------------------------------------
s3_aguardando_ids <- interv_aguardando_sessao(sessao = 3)
s3_aguardando_n <- length(s3_aguardando_ids)


# Sessão 4 =====================================================================
## Aguardando agendamento --------------------------------------
interv_s4_aguard_agend_ids <- interv_aguardando_agendamento(sessao = 4)
interv_s4_aguard_agend_n <- length(interv_s4_aguard_agend_ids)

## Aguardando sessão -------------------------------------------
s4_aguardando_ids <- interv_aguardando_sessao(sessao = 4)
s4_aguardando_n <- length(s4_aguardando_ids)


# Sessão 5 =====================================================================
## Aguardando agendamento --------------------------------------
interv_s5_aguard_agend_ids <- interv_aguardando_agendamento(sessao = 5)
interv_s5_aguard_agend_n <- length(interv_s5_aguard_agend_ids)

## Aguardando sessão -------------------------------------------
s5_aguardando_ids <- interv_aguardando_sessao(sessao = 5)
s5_aguardando_n <- length(s5_aguardando_ids)



# Sessão F =====================================================================
## Finalizadas ---------------------------------------------
# interv_sf_realiz_ids <- df |>
#   filter(desfecho_participante_interv == "Completou a participação") |>
#   select(record_id) |>
#   full_join(
#     df |>
#       filter(
#         redcap_event_name == "Sessao final (Arm 1: Participantes)" &
#           (encerramento_sesso_complete == "Complete" |  # checagem original
#              # !is.na(enc_sessao_agend_data)
#              # checklist_de_sesso_complete == "Complete" |
#              !is.na(enc_sessao_superv))
#       ) |>
#       select(record_id),
#     by = "record_id"
#   ) |>
#   distinct(record_id) |>
#   pull()

interv_sf_realiz_ids <- interv_andamento_df |>
  filter(sessao_final_realizada == 1) |>
  pull(record_id)

# interv_sf_realiz_ids <- df |>
#   filter(
#     redcap_event_name == "Sessao final (Arm 1: Participantes)" &
#       !is.na(score_gad_7)
#   ) |>
#   distinct(record_id) |>
#   pull()

interv_sf_realiz_n <- length(interv_sf_realiz_ids)

## Aguardando agendamento ---------------------------------
interv_sf_aguard_agend_ids <- interv_aguardando_agendamento(sessao = "final")
interv_sf_aguard_agend_n <- length(interv_sf_aguard_agend_ids)

## Aguardando sessão -------------------------------------------
sf_aguardando_ids <- interv_aguardando_sessao(sessao = "final")
sf_aguardando_n <- length(sf_aguardando_ids)



## PSYCHLOPS -----------------------------------------------
df |>
  filter(redcap_event_name == "Sessao final (Arm 1: Participantes)") |>
  with(rstatix::freq_table(psychlops_q6_1_sessao_final))


# Geral ================================================================
## Interromperam  -------------------------------------------
# interv_interromperam_ids <- df |>
#   filter(redcap_event_name == "Agendamento (Arm 1: Participantes)") |>
#   # slice_max(redcap, by = "record_id")
#   filter(sessao_agend == "Não") |>
#   distinct(record_id) |>
#   pull()
# 
# interv_interromperam_n <- length(interv_interromperam_ids)

interv_interromperam_ids <- df |>
  filter(record_id %in% interv_sa_realiz_ids) |>
  inner_join(
    df |>
      filter(redcap_event_name == "Sessao 1 (Arm 1: Participantes)"  &
               !is.na(gad7_perg_1)) |>
      select(record_id),
    by = "record_id"
  ) |>
  filter(
    redcap_event_name == "Desfecho (Arm 1: Participantes)",
    desfecho_participante_interv == "Retirado"
  ) |>
  distinct(record_id) |>
  pull()

interv_interromperam_n <- length(interv_interromperam_ids)

interv_interromperam_str <- df |>
  filter(record_id %in% interv_sa_realiz_ids) |>
  inner_join(
    df |>
      filter(redcap_event_name == "Sessao 1 (Arm 1: Participantes)"  &
               !is.na(gad7_perg_1)) |>
      select(record_id),
    by = "record_id"
  ) |>
  filter(
    redcap_event_name == "Desfecho (Arm 1: Participantes)",
    desfecho_participante_interv == "Retirado"
  ) |>
  with(rstatix::freq_table(desfecho_participante_motivo_interv)) |>
  arrange(group) |>
  mutate(linha = glue("{group} = {n}")) |>
  pull(linha) |>
  paste(collapse = "\n")



# interv_n_exclusao <- df |>
#   # group_by(redcap_event_name) |>
#   filter(
#     enc_sessao_agend_data == "Não" |
#       enc_sessao_superv_apto == "2 - Não"
#   ) |>
#   distinct(record_id) |>
#   nrow()
# 
# # incluir enc_sa_motivo




# Gravações ====================================================================