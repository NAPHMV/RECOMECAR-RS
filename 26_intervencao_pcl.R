# DATAFRAMES ==================================================================
## Geral ------------------------------------------------------
pcl_df <- df |>
  filter(
    redcap_event_name %in% c(
      "Triagem (Arm 1: Participantes)",
      'Sessao de apresentação (Arm 1: Participantes)',
      "Sessao final (Arm 1: Participantes)",
      "Seguimento 3m (Arm 1: Participantes)",
      "Seguimento 6m (Arm 1: Participantes)")
  ) |>
  select(record_id, redcap_event_name, score_pcl_5) |>
  filter(!is.na(score_pcl_5)) |>
  mutate(
    sessao = factor(
      case_when(
        redcap_event_name == "Triagem (Arm 1: Participantes)"  ~ "Triagem",
        redcap_event_name == 'Sessao de apresentação (Arm 1: Participantes)' ~ "Sessão A",
        redcap_event_name == 'Sessao de apresentação (Arm 1: Participantes)' ~ "Sessão A",
        redcap_event_name == "Sessao final (Arm 1: Participantes)"  ~ "Sessão F",
        redcap_event_name == "Seguimento 3m (Arm 1: Participantes)" ~ "3 meses",
        redcap_event_name == "Seguimento 6m (Arm 1: Participantes)" ~ "6 meses",
      )),
    sessao = fct_relevel(sessao, "Triagem", "Sessão A", "Sessão F", "3 meses", "6 meses")
  ) |>
  select(record_id, sessao, escore = score_pcl_5)

## Resumos -----------------------------------------------------
### Todos
pcl_summ_df <- pcl_df |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE),
    n_ids = dplyr::n_distinct(record_id)
  )
pcl_mean_df <- pcl_df |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2)
  )
### Concluíram intervenção
pcl_summ_completos_interv_df <- pcl_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE),
    n_ids = dplyr::n_distinct(record_id)
  )
pcl_mean_completos_interv_df <- pcl_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2)
  )
### Concluíram seguimento
pcl_summ_completos_df <- pcl_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE),
    n_ids = dplyr::n_distinct(record_id)
  )
pcl_mean_completos_df <- pcl_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2)
  )


# N ============================================================================
pcl_tri_n <- pcl_df |>
  filter(sessao == "Triagem") |>
  nrow()
pcl_sa_n <- pcl_df |>
  filter(sessao == "Sessão A") |>
  nrow()
pcl_sf_n <- pcl_df |>
  filter(sessao == "Sessão F") |>
  nrow()
pcl_3m_n <- pcl_df |>
  filter(sessao == "3 meses") |>
  nrow()
pcl_6m_n <- pcl_df |>
  filter(sessao == "6 meses") |>
  nrow()

# STRINGS ======================================================================
## Completo -------------------------------------------------------
### Todos ------------------------------------------
#### Triagem
pcl_tri_str <- pcl_df |>
  filter(sessao == "Triagem") |>
  summarise(
    media_tri  = mean(escore, na.rm = TRUE),
    median_tri = median(escore, na.rm = TRUE),
    sd_tri     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_tri = glue::glue("{round(media_tri, 2)} ({round(sd_tri, 2)})")
  ) |> 
  pull(res_tri)
#### Sessão A
pcl_sa_str <- pcl_df |>
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
#### Sessão Final
pcl_sf_str <- pcl_df |>
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
pcl_3m_str <- pcl_df |>
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
### 6 meses
pcl_6m_str <- pcl_df |>
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

### Filtrado ------------------------------------------
#### Triagem
pcl_tri_filtr_str <- pcl_df |>
  filter(sessao == "Triagem" & record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_tri  = mean(escore, na.rm = TRUE),
    median_tri = median(escore, na.rm = TRUE),
    sd_tri     = sd(escore, na.rm = TRUE)
  ) |>
  reframe(
    res_tri = glue::glue("{round(media_tri, 2)} ({round(sd_tri, 2)})")
  ) |> 
  pull(res_tri)
#### Sessão A
pcl_sa_filtr_str <- pcl_df |>
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
#### Sessão Final
pcl_sf_filtr_str <- pcl_df |>
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
pcl_3m_filtr_str <- pcl_df |>
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
### 6 meses
pcl_6m_filtr_str <- pcl_df |>
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


## Intervenção -------------------------------------------------------------
#### Sessão A
pcl_sa_filtr_interv_str <- pcl_df |>
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
#### Sessão Final
pcl_sf_filtr_interv_str <- pcl_df |>
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



# EFFECT SIZE ==================================================================
## Completo ------------------------------------------------------
# Baseado en https://pmc.ncbi.nlm.nih.gov/articles/PMC3840331/
pcl_effect_size <- pcl_df |>
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
pcl_effect_size2 <- pcl_df |>
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
    effect_size_mean = round(mean(effect_size),2 )
  ) |>
  pull()


## Intervenção ------------------------------------------------------
# Baseado en https://pmc.ncbi.nlm.nih.gov/articles/PMC3840331/
pcl_interv_effect_size <- pcl_df |>
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
pcl_interv_effect_size2 <- pcl_df |>
  pivot_wider(id_cols = record_id, names_from = sessao, values_from = escore) |>
  filter(!is.na(`Sessão A`) & !is.na(`Sessão F`)) |>
  select(record_id, `Sessão A`, `Sessão F`) |>
  mutate(
    score_diff  = `Sessão A` - `Sessão F`,
    sd_sa       = sd(`Sessão A`),
    effect_size = score_diff/sd_sa
  ) |>
  summarise(
    effect_size_mean = round(mean(effect_size),2 )
  ) |>
  pull()


# TABELAS ======================================================================
## Completo --------------------------------------------------------
tabela_pcl <- tibble(
  Triagem         = pcl_tri_str,
  `Sessão A`      = pcl_sa_str,
  `Sessão F`      = pcl_sf_str,
  `3 meses`       = pcl_3m_str,
  `6 meses`       = pcl_6m_str
)
tabela_pcl_filtr <- tibble(
  `Triagem`       = pcl_tri_filtr_str,
  `Sessão A`      = pcl_sa_filtr_str,
  `Sessão F`      = pcl_sf_filtr_str,
  `3 meses`       = pcl_3m_filtr_str,
  `6 meses`       = pcl_6m_filtr_str,
) |>
  mutate(
    `Cohen's dz`  = pcl_effect_size,
    `Effect Size` = pcl_effect_size2
  )

## Intervenção ------------------------------------------------------
tabela_pcl_filtr_interv <- tibble(
  `Sessão A`      = pcl_sa_filtr_interv_str,
  `Sessão F`      = pcl_sf_filtr_interv_str,
) |>
  mutate(
    `Cohen's dz`  = pcl_interv_effect_size,
    `Effect Size` = pcl_interv_effect_size2
  )

