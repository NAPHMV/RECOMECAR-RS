# Atendimento ==================================================================
## Início --------------------------------------------------
facilit_inicio_data <- interv_andamento_df |>
  filter(!is.na(sessao_A_data) & sessao_A_realizada == 1) |>
  select(record_id, sessao_A_data) |>
  left_join(
    df |>
      filter(str_detect(redcap_event_name, "apresenta")) |>
      mutate(
        cod_pesq = case_when(
          is.na(tentativa_cod_pesq_1) ~ NA,
          !is.na(tentativa_cod_pesq_1) &
            is.na(tentativa_cod_pesq_2) ~ tentativa_cod_pesq_1,
          !is.na(tentativa_cod_pesq_2) &
            is.na(tentativa_cod_pesq_3) ~ tentativa_cod_pesq_2,
          !is.na(tentativa_cod_pesq_3) &
            is.na(tentativa_cod_pesq_4) ~ tentativa_cod_pesq_3,
          !is.na(tentativa_cod_pesq_4) &
            is.na(tentativa_cod_pesq_5) ~ tentativa_cod_pesq_4,
          !is.na(tentativa_cod_pesq_5) &
            is.na(tentativa_cod_pesq_6) ~ tentativa_cod_pesq_5,
          !is.na(tentativa_cod_pesq_6) ~ tentativa_cod_pesq_6
        )
      ) |>
      select(record_id, cod_pesq),
    by = "record_id"
  ) |>
  filter(!is.na(cod_pesq)) |>
  group_by(cod_pesq) |>
  slice_min(sessao_A_data) |>
  select(cod_pesq, sessao_A_data)

## n Atendimentos -------------------------------------
facilit_atend_n <- df |>
  filter(str_detect(redcap_event_name, "Sessao")) |>
  mutate(
    cod_pesq = case_when(
      is.na(tentativa_cod_pesq_1) ~ NA,
      !is.na(tentativa_cod_pesq_1) &
        is.na(tentativa_cod_pesq_2) ~ tentativa_cod_pesq_1,
      !is.na(tentativa_cod_pesq_2) &
        is.na(tentativa_cod_pesq_3) ~ tentativa_cod_pesq_2,
      !is.na(tentativa_cod_pesq_3) &
        is.na(tentativa_cod_pesq_4) ~ tentativa_cod_pesq_3,
      !is.na(tentativa_cod_pesq_4) &
        is.na(tentativa_cod_pesq_5) ~ tentativa_cod_pesq_4,
      !is.na(tentativa_cod_pesq_5) &
        is.na(tentativa_cod_pesq_6) ~ tentativa_cod_pesq_5,
      !is.na(tentativa_cod_pesq_6) ~ tentativa_cod_pesq_6
    )
  ) |>
  filter(!is.na(cod_pesq)) |>
  select(cod_pesq, record_id, redcap_event_name) |>
  left_join(
    interv_andamento_df |>
      select(record_id, contains("realizada")) |>
      pivot_longer(cols = -"record_id", names_to = "redcap_event_name", values_to = "realiz") |>
      mutate(
        redcap_event_name = case_when(
          str_detect(redcap_event_name, "sessao_A") ~ "Sessao de apresentação (Arm 1: Participantes)",
          str_detect(redcap_event_name, "sessao_1") ~ "Sessao 1 (Arm 1: Participantes)",
          str_detect(redcap_event_name, "sessao_2") ~ "Sessao 2 (Arm 1: Participantes)",
          str_detect(redcap_event_name, "sessao_3") ~ "Sessao 3 (Arm 1: Participantes)",
          str_detect(redcap_event_name, "sessao_4") ~ "Sessao 4 (Arm 1: Participantes)",
          str_detect(redcap_event_name, "sessao_5") ~ "Sessao 5 (Arm 1: Participantes)",
          str_detect(redcap_event_name, "sessao_f") ~ "Sessao final (Arm 1: Participantes)"
        ),
        realiz = case_when(
          realiz == 1 ~ TRUE, 
          realiz == 0 ~ FALSE,
          TRUE ~ NA
        )
      ),
    by = c("record_id", "redcap_event_name")
  ) |>
  filter(realiz == TRUE) |>
  group_by(cod_pesq) |>
  summarise(atend_n = n_distinct(record_id, redcap_event_name))
  

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
      facilit_segue_estudo == "Não" ~ "Saiu do estudo",
      facilit_segue_estudo == "Sim" &
        is.na(whodas_20_timestamp)  ~ "Indisponível",
      facilit_segue_estudo == "Sim" & 
        !is.na(whodas_20_timestamp) ~ as.character(as.Date(whodas_20_timestamp)),
      TRUE ~ NA
    )
  ) |>
  pivot_wider(id_cols = 'record_id', names_from = 'redcap_event_name', 
              values_from = 'whodas_20_timestamp', names_expand = TRUE) |>
  mutate(
    `6 meses`  = if_else(`3 meses` == "Saiu do estudo", "", `6 meses`),
    `9 meses`  = if_else(`6 meses` == "Saiu do estudo" | `6 meses` == "", "", `9 meses`),
    `12 meses` = if_else(`9 meses` == "Saiu do estudo" | `9 meses` == "", "", `12 meses`),
    # Datas previstas
    `6 meses` = case_when(
      is.na(`6 meses`) & str_detect(`3 meses`, "^\\d{4}-\\d{2}-\\d{2}$") ~ 
        paste("Previsão:", as.Date(`3 meses`, format = "%Y-%m-%d") + 90),
      TRUE ~ `6 meses`
    ),
    `9 meses` = case_when(
      is.na(`9 meses`) & str_detect(`6 meses`, "^\\d{4}-\\d{2}-\\d{2}$") ~ 
        paste("Previsão:", as.Date(`6 meses`, format = "%Y-%m-%d") + 90),
      is.na(`9 meses`) & str_detect(`3 meses`, "^\\d{4}-\\d{2}-\\d{2}$") ~ 
        paste("Previsão:", as.Date(`3 meses`, format = "%Y-%m-%d") + 180),
      TRUE ~ `9 meses`
    ),
    `12 meses` = case_when(
      is.na(`12 meses`) & str_detect(`9 meses`, "^\\d{4}-\\d{2}-\\d{2}$") ~ 
        paste("Previsão:", as.Date(`9 meses`, format = "%Y-%m-%d") + 90),
      is.na(`12 meses`) & str_detect(`6 meses`, "^\\d{4}-\\d{2}-\\d{2}$") ~ 
        paste("Previsão:", as.Date(`6 meses`, format = "%Y-%m-%d") + 180),
      is.na(`12 meses`) & str_detect(`3 meses`, "^\\d{4}-\\d{2}-\\d{2}$") ~ 
        paste("Previsão:", as.Date(`3 meses`, format = "%Y-%m-%d") + 270),
      TRUE ~ `12 meses`
    )
  ) |>
  rename(ID = record_id)


# PHQ-9 =======================================================================
facilit_manejo_phq9 <- df |>
  filter(
    (redcap_event_name == "Durante o Treinamento (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 3m (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 6m (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 9m (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 12m (Arm 2: Facilitadores)") &
      (!phq9_perg_9 %in% "Nenhuma vez") & !is.na(phq9_perg_9)
  ) |>
  group_by(record_id) |>
  summarise(
    momento = case_when(
      redcap_event_name %in% "Durante o Treinamento (Arm 2: Facilitadores)" ~ "Treinamento",
      TRUE ~ "Acompanhamento"),
    data = coalesce(as.Date(phq9_dta_preenchi), as.Date(phq9_timestamp))
  ) |>
  rename(
    ID = record_id,
    `Momento da pontuação` = momento,
    `Data` = data
  )


# Dados ========================================================================
## Sociodemográficos ------------------------------------------
facilit_dados_socio <- df |>
  filter(redcap_event_name == "Baseline (Arm 2: Facilitadores)") |>
  select(
    ID = record_id,
    data_nasc_facilitador, genero_facilitador, raca_facilitador,
    escolaridade_facilitador, area_est_facilitador, 
    outra_area_est_facilitador, trab_volunt_facilitador,
    sair_casa_facilitador
  ) |>
  mutate(
    area_est_facilitador = as.character(area_est_facilitador),
    area_est_facilitador = case_when(
      str_detect(area_est_facilitador, "Outro") ~ glue::glue("Outro: {outra_area_est_facilitador}"),
      TRUE ~ area_est_facilitador
    ),
    data_nasc_facilitador = as.Date(data_nasc_facilitador)
  ) |>
  select(-outra_area_est_facilitador) |>
  rename(
    `Data de nascimento` = data_nasc_facilitador,
    `Gênero` = genero_facilitador,
    `Cor/raça` = raca_facilitador,
    Escolaridade = escolaridade_facilitador,
    `Formação` = area_est_facilitador,
    `Trabalho voluntário` = trab_volunt_facilitador,
    `Saiu/dormiu fora de casa` = sair_casa_facilitador
  )

## PHQ-9 --------------------------------------------------
facilit_dados_phq <- df |>
  filter(str_detect(redcap_event_name, "Arm 2") & !is.na(score_phq_9)) |>
  select(record_id, redcap_event_name, score_phq_9) |>
  mutate(
    redcap_event_name = factor(
      case_when(
        str_detect(redcap_event_name, "Treinamento") ~ "Treinamento",
        str_detect(redcap_event_name, "3m")          ~ "3 meses",
        str_detect(redcap_event_name, "6m")          ~ "6 meses",
        str_detect(redcap_event_name, "9m")          ~ "9 meses",
        str_detect(redcap_event_name, "12m")         ~ "12 meses",
        TRUE ~ NA_character_
      ),
      # Define todos os níveis explicitamente na criação do factor
      levels = c("Treinamento", "3 meses", "6 meses", "9 meses", "12 meses")
    )
  ) |>
  pivot_wider(
    id_cols = "record_id", names_from = "redcap_event_name", 
    values_from = "score_phq_9", names_expand = TRUE) |>
  rename(ID = record_id)
## GAD-7 --------------------------------------------------
facilit_dados_gad <- df |>
  filter(str_detect(redcap_event_name, "Arm 2") & !is.na(score_gad_7)) |>
  select(record_id, redcap_event_name, score_gad_7) |>
  mutate(
    redcap_event_name = factor(
      case_when(
        str_detect(redcap_event_name, "Treinamento") ~ "Treinamento",
        str_detect(redcap_event_name, "3m")          ~ "3 meses",
        str_detect(redcap_event_name, "6m")          ~ "6 meses",
        str_detect(redcap_event_name, "9m")          ~ "9 meses",
        str_detect(redcap_event_name, "12m")         ~ "12 meses",
        TRUE ~ NA_character_
      ),
      # Define todos os níveis explicitamente na criação do factor
      levels = c("Treinamento", "3 meses", "6 meses", "9 meses", "12 meses")
    )
  ) |>
  pivot_wider(
    id_cols = "record_id", names_from = "redcap_event_name", 
    values_from = "score_gad_7", names_expand = TRUE) |>
  rename(ID = record_id)
## PSS -------------------------------------------
facilit_dados_pss <- df |>
  filter(str_detect(redcap_event_name, "Arm 2")) |>
  select(
    record_id, redcap_event_name, pss_q1:pss_q5
  ) |>
  mutate(
    redcap_event_name = factor(
      case_when(
        str_detect(redcap_event_name, "Treinamento") ~ "Treinamento",
        str_detect(redcap_event_name, "3m")          ~ "3 meses",
        str_detect(redcap_event_name, "6m")          ~ "6 meses",
        str_detect(redcap_event_name, "9m")          ~ "9 meses",
        str_detect(redcap_event_name, "12m")         ~ "12 meses",
        TRUE ~ NA_character_
      ),
      # Define todos os níveis explicitamente na criação do factor
      levels = c("Treinamento", "3 meses", "6 meses", "9 meses", "12 meses")
    )
  ) |>
  filter(!is.na(redcap_event_name) & !if_all(contains("pss"), is.na)) |>
  rename(ID = record_id, Etapa = redcap_event_name) |>
  rename_with(.cols = contains("pss"), .fn = \(x) paste("Questão ", substring(x, nchar(x))))
