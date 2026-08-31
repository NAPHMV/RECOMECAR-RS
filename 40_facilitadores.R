# IDs válidos ==================================================================
facilit_codigos_invalidos <- c(as.character(c(
  5, 9, 21, 26, 29, 30, 42, 48:53
)), "05", "09")

facilit_ids <- df |>
  filter(!is.na(id_facilitador)) |>
  select(record_id, id_facilitador) |>
  filter(!id_facilitador %in% facilit_codigos_invalidos) |>
  distinct(record_id) |>
  pull()


# Ativos =======================================================================
googlesheets4::gs4_deauth()
dados_seg_facilit <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1MJaDZM8KAWofWWtFC7TvvlgBtY7vmOGq8X2ZM-lLCLg/edit?gid=0#gid=0") |>
  select(
    `Código` = `Código do facilitador`,
    ID = ID,
    Onda = `FASE DE INICIO`,
    Baseline,
    `3M`,
    `6M`,
    `9M`,
    `12M`,
  ) |>
  group_by(Onda) |>
  summarise(
    across(c("Baseline", matches("M")),
           \(x) sum(x == "Concluído", na.rm = TRUE)),
    `3 meses` = glue::glue("{`3M`}/{Baseline}"),
    `6 meses` = glue::glue("{`6M`}/{`3M`}"),
    `9 meses` = glue::glue("{`9M`}/{`6M`}"),
    `12 meses` = glue::glue("{`12M`}/{`9M`}")
  ) |>
  select(-c(`3M`:`12M`))


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
  filter(!(cod_pesq %in% facilit_codigos_invalidos)) |>
  select(
    `Código` = cod_pesq, 
    `Primeira Sessão A` = sessao_A_data)

## n Atendimentos -------------------------------------
facilit_atend_n <- df |>
  ## Atendimentos na Intervenção
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
  summarise(interv_atend_n = n_distinct(record_id, redcap_event_name)) |>
  ## Atendimento na Pré-Triagem
  full_join(
    df |>
      filter(str_detect(redcap_event_name, "Triagem")) |>
      mutate(
        cod_pesq = case_when(
          is.na(cod_pesq_1_busc) ~ NA,
          !is.na(cod_pesq_1_busc) &
            is.na(cod_pesq_2_busc) ~ cod_pesq_1_busc,
          !is.na(cod_pesq_2_busc) &
            is.na(cod_pesq_3_busc) ~ cod_pesq_2_busc,
          !is.na(cod_pesq_3_busc) &
            is.na(cod_pesq_4_busc) ~ cod_pesq_3_busc,
          !is.na(cod_pesq_4_busc) &
            is.na(cod_pesq_5_busc) ~ cod_pesq_4_busc,
          !is.na(cod_pesq_5_busc) &
            is.na(cod_pesq_6_busc) ~ cod_pesq_5_busc,
          !is.na(cod_pesq_6_busc) ~ cod_pesq_6_busc
        )
      ) |>
      filter(!is.na(cod_pesq) & record_id %in% pretri_realiz_ids) |>
      group_by(cod_pesq) |>
      summarise(pretri_atend_n = dplyr::n()),
    by = "cod_pesq"
  ) |>
  ## Atendimentos Triagem
  full_join(
    df |>
      filter(str_detect(redcap_event_name, "Triagem")) |>
      mutate(
        cod_pesq = case_when(
          is.na(cod_pesq_1) ~ NA,
          !is.na(cod_pesq_1) &
            is.na(cod_pesq_2) ~ cod_pesq_1,
          !is.na(cod_pesq_2) &
            is.na(cod_pesq_3) ~ cod_pesq_2,
          !is.na(cod_pesq_3) &
            is.na(cod_pesq_4) ~ cod_pesq_3,
          !is.na(cod_pesq_4) &
            is.na(cod_pesq_5) ~ cod_pesq_4,
          !is.na(cod_pesq_5) &
            is.na(cod_pesq_6) ~ cod_pesq_5,
          !is.na(cod_pesq_6) ~ cod_pesq_6
        )
      ) |>
      filter(!is.na(cod_pesq) & record_id %in% tri_realiz_ids) |>
      group_by(cod_pesq) |>
      summarise(tri_atend_n = dplyr::n()),
    by = "cod_pesq"
  ) |>
  filter(!(cod_pesq %in% facilit_codigos_invalidos)) |>
  rowwise() |>
  mutate(
    # cod_pesq = as.numeric(cod_pesq),
    soma = sum(c(pretri_atend_n, tri_atend_n, interv_atend_n), na.rm = TRUE)
  ) |>
  select(
    `Código` = cod_pesq,
    `Pré-triagem` = pretri_atend_n,
    Triagem = tri_atend_n,
    `Intervenção` = interv_atend_n,
    Total = soma
  ) |>
  arrange(`Código`)
  

# Consentimento ================================================================
facilit_seg_andamento_consent <- df |>
  filter(
    redcap_event_name == "Baseline (Arm 2: Facilitadores)" &
      tcle_consentiu_questionarios == "Sim" &
      record_id %in% facilit_ids
  ) |>
  reframe(ID = record_id, Consentimento = as.Date(data_preenchi_facili))


# Andamento ====================================================================
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


# Manejo =======================================================================
facilit_manejo_phq9 <- df |>
  filter(
    (redcap_event_name == "Durante o Treinamento (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 3m (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 6m (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 9m (Arm 2: Facilitadores)" |
       redcap_event_name == "Seguimento 12m (Arm 2: Facilitadores)") &
      (!phq9_perg_9 %in% "Nenhuma vez") & !is.na(phq9_perg_9) &
      record_id %in% facilit_ids
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
  filter(
    redcap_event_name == "Baseline (Arm 2: Facilitadores)" &
      record_id %in% facilit_ids) |>
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
  filter(
    str_detect(redcap_event_name, "Arm 2") & 
      !is.na(score_phq_9) &
      record_id %in% facilit_ids
  ) |>
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
  filter(
    str_detect(redcap_event_name, "Arm 2") & 
      !is.na(score_gad_7) &
      record_id %in% facilit_ids
  ) |>
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
  filter(
    str_detect(redcap_event_name, "Arm 2") &
      record_id %in% facilit_ids
  ) |>
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

# Tabelas --------------------------------------------------
## Sociodemográficos --------------------------
facilit_tab1_build <- function(df) {
  df |>
  filter(
    redcap_event_name == "Baseline (Arm 2: Facilitadores)" &
      record_id %in% facilit_ids
  ) |>
  select(
    idade_facilitador, genero_facilitador, raca_facilitador,
    escolaridade_facilitador, area_est_facilitador, 
    outra_area_est_facilitador, trab_volunt_facilitador,
    sair_casa_facilitador
  ) |>
  mutate(
    genero_facilitador = case_when(
      str_detect(genero_facilitador, "Outro") ~ "Outro", 
      str_detect(genero_facilitador, "Prefiro") ~ "Preferiu não responder",
      TRUE ~ genero_facilitador
    ),
    area_est_facilitador = case_when(
      str_detect(area_est_facilitador, "Outro") ~ "Outra área",
      TRUE ~ area_est_facilitador
    )
  ) |>
  select(-outra_area_est_facilitador) |>
  tbl_summary(
    label = list(
      idade_facilitador ~ "Idade",
      genero_facilitador ~ "Gênero",
      raca_facilitador ~ "Raça/cor",
      escolaridade_facilitador ~ "Escolaridade",
      area_est_facilitador ~ "Área de estudo",
      trab_volunt_facilitador ~ "Trabalho voluntário",
      sair_casa_facilitador ~ "Precisou sair/dormir fora de casa"
    ),
    type = list(
      trab_volunt_facilitador ~ "dichotomous",
      sair_casa_facilitador ~ "dichotomous"
    ),
    value = list(
      trab_volunt_facilitador ~ "Sim",
      sair_casa_facilitador ~ "Sim"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      idade_facilitador ~ "{mean} ± {sd}"
    ),
    digits = list(
      all_continuous()  ~ 1,
      all_categorical() ~ c(0, 0, 2)
    )
  ) |>
  modify_header(label = "**Variável**", stat_0 = "**n = {N}**") |>
  modify_footnote(stat_0 ~ "Média ± DP; Mediana [Q1-Q3]; n (%)") |>
  bold_labels() |>
  as_gt() |>
  tab_header(
    title = html("<strong>Tabela 1.</strong> Dados sociodemográficos e experiência durante enchentes.")
  ) |>
  tab_options(
    table.font.size = px(13),
    table.font.names = "Times New Roman",
    heading.title.font.size = px(13),   
    heading.align = "left",
    row_group.border.top.color = "grey90",
    row_group.border.top.width = px(1),
    row_group.border.bottom.color = "grey90",
    row_group.border.bottom.width = px(.5),
    row_group.padding = px(2)          # reduz o espaço vertical dos grupos
  ) |>
  # Negrito nos nomes dos grupos
  tab_style(
    style = cell_text(size = px(13), weight = "normal"),
    locations = cells_row_groups()
  ) |>
  # Tamanho de texto das linhas
  tab_style(
    style = cell_text(size = px(13), weight = "normal"),
    locations = cells_body()
  )
}

## Escores ----------------------------------------
facilit_tab2_build <- function(df) {
  df |>
    filter(
      str_detect(redcap_event_name, "Arm 2") &
        record_id %in% facilit_ids
    ) |>
    select(redcap_event_name, score_phq_9, score_gad_7, pss_q1:pss_q5) |>
    mutate(
      redcap_event_name = factor(
        case_when(
          str_detect(redcap_event_name, "Durante") ~ "Treinamento",
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
    rename(Etapa = redcap_event_name) |>
    # rename_with(.cols = contains("pss"), .fn = \(x) paste("Questão ", substring(x, nchar(x)))) |>
    tbl_summary(
      by = Etapa,
      label = list(
        score_phq_9 ~ "PHQ-9",
        score_gad_7 ~ "GAD-7"
      ),
      statistic = all_categorical() ~ "{n} ({p}%)",
      digits = list(
        all_continuous()  ~ 1,
        all_categorical() ~ c(0, 0, 2)
      ),
      missing = "no"
    ) |>
    modify_table_body(
      ~ .x |>
        mutate(across(
          all_stat_cols(),
          \(x) str_replace_all(x, "0 \\(NA%\\)|NA \\(NA, NA\\)|NA \\(NA%\\)", "—")
        ))
    ) |>
    modify_header(label = "**Variável**") |>
    # modify_footnote(stat_0 ~ "Média ± DP; Mediana [Q1-Q3]; n (%)") |>
    # bold_labels() |>
    as_gt() |>
    tab_row_group(
      label = "PSS",
      rows = variable %in% c("pss_q1", "pss_q2", "pss_q3", "pss_q4", "pss_q5")
    ) |>
    tab_row_group(
      label = "Escalas",
      rows = variable %in% c("score_phq_9", "score_gad_7")
    ) |>
    tab_header(
      title = html("<strong>Tabela 2.</strong> Pontuação em escalas durante o treinamento e ao longo do seguimento.")
    ) |>
    tab_options(
      table.font.size = px(13),
      table.font.names = "Times New Roman",
      heading.title.font.size = px(13),
      heading.align = "left",
      row_group.border.top.color = "grey70",
      # row_group.border.top.width = px(1),
      row_group.border.bottom.color = "grey90"
      # row_group.border.bottom.width = px(.5),
      # row_group.padding = px(2) # reduz o espaço vertical dos grupos
    ) |>
    # Negrito nos nomes dos grupos
    tab_style(
      style = cell_text(size = px(13), weight = "bold"),
      locations = cells_row_groups()
    ) |>
    # Tamanho de texto das linhas
    tab_style(
      style = cell_text(size = px(13), weight = "normal"),
      locations = cells_body()
    )
}



