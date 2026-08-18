# DATAFRAMES ===================================================================
## Geral -------------------------------------------------------------
gad_df <- df |>
  filter(
    # !redcap_event_name %in% c(
    #   "Triagem (Arm 1: Participantes)"),
    # !grepl("Seguimento", redcap_event_name),
    !grepl("Arm 2", redcap_event_name)
  ) |>
  select(record_id, redcap_event_name, score_gad_7) |>
  filter(!is.na(score_gad_7)) |>
  bind_rows(
    df |>
      filter(
        redcap_event_name %in% c("Sessao 1 (Arm 1: Participantes)",
                                 redcap_event_name == "Sessao 2 (Arm 1: Participantes)",
                                 redcap_event_name == "Sessao 3 (Arm 1: Participantes)",
                                 redcap_event_name == "Sessao 4 (Arm 1: Participantes)",
                                 redcap_event_name == "Sessao 5 (Arm 1: Participantes)")
      ) |>
      group_by(record_id) |>
      summarise(
        score_gad_7 = mean(score_gad_7, na.rm = TRUE)
      ) |>
      mutate(
        redcap_event_name = "Sessões 1 a 5"
      ) |>
      select(record_id, redcap_event_name, score_gad_7)
  ) |>
  mutate(
    sessao = factor(
      case_when(
        redcap_event_name == "Triagem (Arm 1: Participantes)"  ~ "Triagem",
        redcap_event_name == 'Sessao de apresentação (Arm 1: Participantes)' ~ "Sessão A",
        redcap_event_name == "Sessao 1 (Arm 1: Participantes)" ~ "Sessão 1",
        redcap_event_name == "Sessao 2 (Arm 1: Participantes)" ~ "Sessão 2",
        redcap_event_name == "Sessao 3 (Arm 1: Participantes)" ~ "Sessão 3",
        redcap_event_name == "Sessao 4 (Arm 1: Participantes)" ~ "Sessão 4",
        redcap_event_name == "Sessao 5 (Arm 1: Participantes)" ~ "Sessão 5",
        redcap_event_name == "Sessao final (Arm 1: Participantes)"  ~ "Sessão F",
        redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" ~ "3 meses",
        redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" ~ "6 meses",
        redcap_event_name == "Sessões 1 a 5" ~ "Sessões 1 a 5"
      )),
    sessao = fct_relevel(
      sessao, "Triagem", "Sessão A", "Sessão 1", "Sessão 2", "Sessão 3",
      "Sessão 4", "Sessão 5", "Sessão F", "3 meses", "6 meses", "Sessões 1 a 5")
  ) |>
  select(record_id, sessao, escore = score_gad_7)


## Resumos -------------------------------------------
### Todos
gad_summ_df <- gad_df |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE)
  )
gad_mean_df <- gad_df |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2)
  )
### Completaram intervenção
gad_summ_completos_df <- gad_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25),
    escore_q3    = quantile(escore, .75)
  )
gad_mean_completos_df <- gad_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2)
  )
### Completaram seguimento
gad_summ_completos_df <- gad_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25),
    escore_q3    = quantile(escore, .75)
  )
gad_mean_completos_df <- gad_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2)
  )


# N =========================================================================
gad_tri_n <- gad_df |>
  filter(sessao == "Triagem") |>
  nrow()

gad_sa_n <- gad_df |>
  filter(sessao == "Sessão A") |>
  nrow()

gad_s1_n <- gad_df |>
  filter(sessao == "Sessão 1") |>
  nrow()

gad_s2_n <- gad_df |>
  filter(sessao == "Sessão 2") |>
  nrow()

gad_s3_n <- gad_df |>
  filter(sessao == "Sessão 3") |>
  nrow()

gad_s4_n <- gad_df |>
  filter(sessao == "Sessão 4") |>
  nrow()

gad_s5_n <- gad_df |>
  filter(sessao == "Sessão 5") |>
  nrow()

gad_sf_n <- gad_df |>
  filter(sessao == "Sessão F") |>
  nrow()

gad_3m_n <- gad_df |>
  filter(sessao == "3 meses") |>
  nrow()

gad_6m_n <- gad_df |>
  filter(sessao == "6 meses") |>
  nrow()



# STRINGS ======================================================================
## Completo -------------------------------------------------------
### Todos -------------------------------------------------
#### Triagem
gad_tri_str <- gad_df |>
  filter(sessao == "Triagem") |>
  summarise(
    media_tri  = mean(escore, na.rm = TRUE),
    median_tri = median(escore, na.rm = TRUE),
    sd_tri     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_tri, 2)} ({round(sd_tri, 2)})")
  ) |> 
  pull(res_sa)
#### Sessão A
gad_sa_str <- gad_df |>
  filter(sessao == "Sessão A") |>
  summarise(
    media_sa  = mean(escore, na.rm = TRUE),
    median_sa = median(escore, na.rm = TRUE),
    sd_sa     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_sa, 2)} ({round(sd_sa, 2)})")
  ) |> 
  pull(res_sa)
#### Sessões 1 a 5
gad_s1_s5_str <- gad_df |>
  filter(sessao == "Sessões 1 a 5") |>
  summarise(
    media_s1_s5  = mean(escore, na.rm = TRUE), 
    median_s1_s5 = median(escore, na.rm = TRUE), 
    sd_s1_s5     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_s1_s5 = glue::glue("{round(media_s1_s5, 2)} ({round(sd_s1_s5,2)})")
  ) |>
  pull(res_s1_s5)
#### Sessão Final
gad_sf_str <- gad_df |>
  filter(sessao == "Sessão F") |>
  summarise(
    media_sf  = mean(escore, na.rm = TRUE),
    median_sf = median(escore, na.rm = TRUE),
    sd_sf     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sf = glue::glue("{round(media_sf, 2)} ({round(sd_sf,2)})")
  ) |>
  pull(res_sf)
#### 3 meses
gad_3m_str <- gad_df |>
  filter(sessao == "3 meses") |>
  summarise(
    media_3m  = mean(escore, na.rm = TRUE),
    median_3m = median(escore, na.rm = TRUE),
    sd_3m     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_3m = glue::glue("{round(media_3m, 2)} ({round(sd_3m,2)})")
  ) |>
  pull(res_3m)
#### 6 meses
gad_6m_str <- gad_df |>
  filter(sessao == "6 meses") |>
  summarise(
    media_6m  = mean(escore, na.rm = TRUE),
    median_6m = median(escore, na.rm = TRUE),
    sd_6m     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_6m = glue::glue("{round(media_6m, 2)} ({round(sd_6m,2)})")
  ) |>
  pull(res_6m)


### Filtrado -------------------------------------------------
#### Triagem
gad_tri_filtr_str <- gad_df |>
  filter(sessao == "Triagem" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_tri  = mean(escore, na.rm = TRUE),
    median_tri = median(escore, na.rm = TRUE),
    sd_tri     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_tri, 2)} ({round(sd_tri, 2)})")
  ) |> 
  pull(res_sa)
#### Sessão A
gad_sa_filtr_str <- gad_df |>
  filter(sessao == "Sessão A" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_sa  = mean(escore, na.rm = TRUE),
    median_sa = median(escore, na.rm = TRUE),
    sd_sa     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_sa, 2)} ({round(sd_sa, 2)})")
  ) |> 
  pull(res_sa)
#### Sessões 1 a 5
gad_s1_s5_filtr_str <- gad_df |>
  filter(sessao == "Sessões 1 a 5" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_s1_s5  = mean(escore, na.rm = TRUE), 
    median_s1_s5 = median(escore, na.rm = TRUE), 
    sd_s1_s5     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_s1_s5 = glue::glue("{round(media_s1_s5, 2)} ({round(sd_s1_s5,2)})")
  ) |>
  pull(res_s1_s5)
#### Sessão Final
gad_sf_filtr_str <- gad_df |>
  filter(sessao == "Sessão F" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_sf  = mean(escore, na.rm = TRUE),
    median_sf = median(escore, na.rm = TRUE),
    sd_sf     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sf = glue::glue("{round(media_sf, 2)} ({round(sd_sf,2)})")
  ) |>
  pull(res_sf)
#### 3 meses
gad_3m_filtr_str <- gad_df |>
  filter(sessao == "3 meses" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_3m  = mean(escore, na.rm = TRUE),
    median_3m = median(escore, na.rm = TRUE),
    sd_3m     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_3m = glue::glue("{round(media_3m, 2)} ({round(sd_3m,2)})")
  ) |>
  pull(res_3m)
#### 6 meses
gad_6m_filtr_str <- gad_df |>
  filter(sessao == "6 meses" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_6m  = mean(escore, na.rm = TRUE),
    median_6m = median(escore, na.rm = TRUE),
    sd_6m     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_6m = glue::glue("{round(media_6m, 2)} ({round(sd_6m,2)})")
  ) |>
  pull(res_6m)


## Intervenção ----------------------------------------------------
#### Triagem
gad_tri_filtr_interv_str <- gad_df |>
  filter(sessao == "Triagem" & record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_tri  = mean(escore, na.rm = TRUE),
    median_tri = median(escore, na.rm = TRUE),
    sd_tri     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_tri, 2)} ({round(sd_tri, 2)})")
  ) |> 
  pull(res_sa)
#### Sessão A
gad_sa_filtr_interv_str <- gad_df |>
  filter(sessao == "Sessão A" & record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_sa  = mean(escore, na.rm = TRUE),
    median_sa = median(escore, na.rm = TRUE),
    sd_sa     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_sa, 2)} ({round(sd_sa, 2)})")
  ) |> 
  pull(res_sa)
#### Sessões 1 a 5
gad_s1_s5_filtr_interv_str <- gad_df |>
  filter(sessao == "Sessões 1 a 5" & record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_s1_s5  = mean(escore, na.rm = TRUE), 
    median_s1_s5 = median(escore, na.rm = TRUE), 
    sd_s1_s5     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_s1_s5 = glue::glue("{round(media_s1_s5, 2)} ({round(sd_s1_s5,2)})")
  ) |>
  pull(res_s1_s5)
#### Sessão Final
gad_sf_filtr_interv_str <- gad_df |>
  filter(sessao == "Sessão F" & record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_sf  = mean(escore, na.rm = TRUE),
    median_sf = median(escore, na.rm = TRUE),
    sd_sf     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_sf = glue::glue("{round(media_sf, 2)} ({round(sd_sf,2)})")
  ) |>
  pull(res_sf)
#### 3 meses
gad_3m_filtr_interv_str <- gad_df |>
  filter(sessao == "3 meses" & record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_3m  = mean(escore, na.rm = TRUE),
    median_3m = median(escore, na.rm = TRUE),
    sd_3m     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_3m = glue::glue("{round(media_3m, 2)} ({round(sd_3m,2)})")
  ) |>
  pull(res_3m)
#### 6 meses
gad_6m_filtr_interv_str <- gad_df |>
  filter(sessao == "6 meses" & record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_6m  = mean(escore, na.rm = TRUE),
    median_6m = median(escore, na.rm = TRUE),
    sd_6m     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_6m = glue::glue("{round(media_6m, 2)} ({round(sd_6m,2)})")
  ) |>
  pull(res_6m)

# EFFECT SIZE ==================================================================
## Completo --------------------------------------------------
# Baseado en https://pmc.ncbi.nlm.nih.gov/articles/PMC3840331/
gad_effect_size <- gad_df |>
  pivot_wider(id_cols = record_id, names_from = sessao, values_from = escore) |>
  filter(!is.na(`Sessão A`) & !is.na(`Sessão F`)) |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  select(record_id, `Sessão A`, `Sessão F`) |>
  mutate(
    score_diff = `Sessão A` - `Sessão F`
  ) |>
  summarise(
    mean_diff = mean(score_diff),
    sd_diff   = sd(score_diff),
    cohen_dz  = round((mean_diff - 0)/(sd_diff), 2)
  ) |>
  pull(cohen_dz)
# Baseado na solicitação
gad_effect_size2 <- gad_df |>
  pivot_wider(id_cols = record_id, names_from = sessao, values_from = escore) |>
  filter(!is.na(`Sessão A`) & !is.na(`Sessão F`)) |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  select(record_id, `Sessão A`, `Sessão F`) |>
  mutate(
    score_diff  = `Sessão A` - `Sessão F`,
    sd_sa       = sd(`Sessão A`),
    effect_size = score_diff/sd_sa
  ) |>
  summarise(
    effect_size_mean = round(mean(effect_size), 2)
  ) |>
  pull()

## Intervenção --------------------------------------------------
# Baseado en https://pmc.ncbi.nlm.nih.gov/articles/PMC3840331/
gad_effect_size <- gad_df |>
  pivot_wider(id_cols = record_id, names_from = sessao, values_from = escore) |>
  filter(!is.na(`Sessão A`) & !is.na(`Sessão F`)) |>
  select(record_id, `Sessão A`, `Sessão F`) |>
  mutate(
    score_diff = `Sessão A` - `Sessão F`
  ) |>
  summarise(
    mean_diff = mean(score_diff),
    sd_diff   = sd(score_diff),
    cohen_dz  = round((mean_diff - 0)/(sd_diff), 2)
  ) |>
  pull(cohen_dz)
# Baseado na solicitação
gad_effect_size2 <- gad_df |>
  pivot_wider(id_cols = record_id, names_from = sessao, values_from = escore) |>
  filter(!is.na(`Sessão A`) & !is.na(`Sessão F`)) |>
  select(record_id, `Sessão A`, `Sessão F`) |>
  mutate(
    score_diff  = `Sessão A` - `Sessão F`,
    sd_sa       = sd(`Sessão A`),
    effect_size = score_diff/sd_sa
  ) |>
  summarise(
    effect_size_mean = round(mean(effect_size), 2)
  ) |>
  pull()

# TABELAS =====================================================================
## Completo -----------------------------------------------
tabela_gad <- tibble(
  Triagem         = gad_tri_str,
  `Sessão A`      = gad_sa_str,
  `Sessões 1 a 5` = gad_s1_s5_str,
  `Sessão F`      = gad_sf_str,
)
tabela_gad_filtr <- tibble(
  Triagem         = gad_filtr_tri_str,
  `Sessão A`      = gad_filtr_sa_str,
  `Sessões 1 a 5` = gad_filtr_s1_s5_str,
  `Sessão F`      = gad_filtr_sf_str
) |>
  mutate(
    `Cohen's dz`  = gad_effect_size,
    `Effect Size` = gad_effect_size2
  )
## Intervenção -----------------------------------------------
tabela_gad_filtr_interv <- tibble(
  `Sessão A`      = gad_sa_filtr_interv_str,
  `Sessões 1 a 5` = gad_s1_s5_filtr_interv_str,
  `Sessão F`      = gad_sf_filtr_interv_str
) |>
  mutate(
    `Cohen's dz`  = gad_interv_effect_size,
    `Effect Size` = gad_interv_effect_size2
  )


# TEXTO ========================================================================
texto_gad <- paste0(
  "<b>O escore é calculado como a soma de questões 1 a 7</b>.<br>
<br>
Nas Sessões 1 a 5, para cada participante, foi primeiro calculado o escore em cada Sessão, e então a média dentre as Sessões foi calculada. <br> 
Escore descrito como <b>Média (Desvio Padrão)</b><br>
<br>
<b>Effect Size</b> foi calculado como média de todos os efeitos, calculados para cada participante por (SA - SF)/(Desvio Padrão SA).<br>
<br>
<b>Cohen's dz</b> foi calculado como (Média da Diferença)/(DP da Diferença).<br>
Referência: https://doi.org/10.3389/fpsyg.2013.00863 (eq. 6)."
)
