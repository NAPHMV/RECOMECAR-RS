# DATAFRAMES ===================================================================
## Geral -------------------------------------------------------------
psychlops_df <- df |>
  filter(redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)") |>
  mutate(
    across(c(psychlops_q1_2, psychlops_q2_2,
             psychlops_q3_2, psychlops_q4_1), 
           \(x) as.numeric(x) - 1)
  ) |>
  group_by(record_id) |>
  mutate(
    psychlops_score_a = case_when(
      !is.na(psychlops_q1_2) & 
        is.na(psychlops_q2_2) ~ sum(
          c(psychlops_q1_2*2,
            psychlops_q3_2, psychlops_q4_1), 
          na.rm = TRUE),
      !is.na(psychlops_q1_2) & 
        !is.na(psychlops_q2_2) ~ sum(
          c(psychlops_q1_2, psychlops_q2_2,
            psychlops_q3_2, psychlops_q4_1), 
          na.rm = TRUE),
      TRUE ~ NA)
  ) |>
  select(record_id, psychlops_score_a) |>
  ungroup() |>
  left_join(
    df |>
      select(record_id, redcap_event_name, contains("psychlops")) |>
      filter(
        redcap_event_name %in% c(
          "Sessao 1 (Arm 1: Participantes)", "Sessao 2 (Arm 1: Participantes)",
          "Sessao 3 (Arm 1: Participantes)", "Sessao 4 (Arm 1: Participantes)",
          "Sessao 5 (Arm 1: Participantes)")
      ) |>
      mutate(
        across(c(psychlops_q1_2_sessao_1, psychlops_q2_2_sessao_1,
                 psychlops_q3_2_sessao_1, psychlops_q4_1_sessao_1,
                 psychlops_q1_2_sessao_2, psychlops_q2_2_sessao_2,
                 psychlops_q3_2_sessao_2, psychlops_q4_1_sessao_2, 
                 psychlops_q1_2_sessao_3, psychlops_q2_2_sessao_3,
                 psychlops_q3_2_sessao_3, psychlops_q4_1_sessao_3, 
                 psychlops_q1_2_sessao_4, psychlops_q2_2_sessao_4,
                 psychlops_q3_2_sessao_4, psychlops_q4_1_sessao_4, 
                 psychlops_q1_2_sessao_5, psychlops_q2_2_sessao_5,
                 psychlops_q3_2_sessao_5, psychlops_q4_1_sessao_5), 
               \(x) as.numeric(x) - 1)
      ) |>
      group_by(record_id) |>
      mutate(
        psychlops_score_1 = case_when(
          !is.na(psychlops_q1_2_sessao_1) & 
            is.na(psychlops_q2_2_sessao_1) ~ sum(
              c(psychlops_q1_2_sessao_1*2,
                psychlops_q3_2_sessao_1, psychlops_q4_1_sessao_1), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_sessao_1) & 
            !is.na(psychlops_q2_2_sessao_1) ~ sum(
              c(psychlops_q1_2_sessao_1, psychlops_q2_2_sessao_1,
                psychlops_q3_2_sessao_1, psychlops_q4_1_sessao_1), 
              na.rm = TRUE),
          TRUE ~ NA
        ),
        psychlops_score_2 = case_when(
          !is.na(psychlops_q1_2_sessao_2) & 
            is.na(psychlops_q2_2_sessao_2) ~ sum(
              c(psychlops_q1_2_sessao_2*2,
                psychlops_q3_2_sessao_2, psychlops_q4_1_sessao_2), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_sessao_2) & 
            !is.na(psychlops_q2_2_sessao_2) ~ sum(
              c(psychlops_q1_2_sessao_2, psychlops_q2_2_sessao_2,
                psychlops_q3_2_sessao_2, psychlops_q4_1_sessao_2), 
              na.rm = TRUE),
          TRUE ~ NA
        ),
        psychlops_score_3 = case_when(
          !is.na(psychlops_q1_2_sessao_3) & 
            is.na(psychlops_q2_2_sessao_3) ~ sum(
              c(psychlops_q1_2_sessao_3*2,
                psychlops_q3_2_sessao_3, psychlops_q4_1_sessao_3), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_sessao_3) & 
            !is.na(psychlops_q2_2_sessao_3) ~ sum(
              c(psychlops_q1_2_sessao_3, psychlops_q2_2_sessao_3,
                psychlops_q3_2_sessao_3, psychlops_q4_1_sessao_3), 
              na.rm = TRUE),
          TRUE ~ NA
        ),
        psychlops_score_4 = case_when(
          !is.na(psychlops_q1_2_sessao_4) & 
            is.na(psychlops_q2_2_sessao_4) ~ sum(
              c(psychlops_q1_2_sessao_4*2,
                psychlops_q3_2_sessao_4, psychlops_q4_1_sessao_4), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_sessao_4) & 
            !is.na(psychlops_q2_2_sessao_4) ~ sum(
              c(psychlops_q1_2_sessao_4, psychlops_q2_2_sessao_4,
                psychlops_q3_2_sessao_4, psychlops_q4_1_sessao_4), 
              na.rm = TRUE),
          TRUE ~ NA
        ),
        psychlops_score_5 = case_when(
          !is.na(psychlops_q1_2_sessao_5) & 
            is.na(psychlops_q2_2_sessao_5) ~ sum(
              c(psychlops_q1_2_sessao_5*2,
                psychlops_q3_2_sessao_5, psychlops_q4_1_sessao_5), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_sessao_5) & 
            !is.na(psychlops_q2_2_sessao_5) ~ sum(
              c(psychlops_q1_2_sessao_5, psychlops_q2_2_sessao_5,
                psychlops_q3_2_sessao_5, psychlops_q4_1_sessao_5), 
              na.rm = TRUE),
          TRUE ~ NA
        ),
        psychlops_score_1_5 = mean(c(psychlops_score_1, psychlops_score_2,
                                     psychlops_score_3, psychlops_score_4,
                                     psychlops_score_5), na.rm = TRUE)
      ) |>
      select(record_id, psychlops_score_1, psychlops_score_2,
             psychlops_score_3, psychlops_score_4,
             psychlops_score_5, psychlops_score_1_5) |>
      ungroup(),
    by = c("record_id")
  ) |>
  left_join(
    df |>
      select(record_id, redcap_event_name, encerramento_sesso_complete, contains("psychlops")) |>
      filter(
        redcap_event_name == "Sessao final (Arm 1: Participantes)",
        record_id %in% interv_sf_realiz_ids
      ) |>
      mutate(
        across(c(psychlops_q1_2_sessao_final, psychlops_q2_2_sessao_final,
                 psychlops_q3_2_sessao_final, psychlops_q4_1_sessao_final), 
               \(x) as.numeric(x) - 1)
      ) |>
      group_by(record_id) |>
      mutate(
        psychlops_score_f = case_when(
          !is.na(psychlops_q1_2_sessao_final) & 
            is.na(psychlops_q2_2_sessao_final) ~ sum(
              c(psychlops_q1_2_sessao_final*2,
                psychlops_q3_2_sessao_final, psychlops_q4_1_sessao_final), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_sessao_final) & 
            !is.na(psychlops_q2_2_sessao_final) ~ sum(
              c(psychlops_q1_2_sessao_final, psychlops_q2_2_sessao_final,
                psychlops_q3_2_sessao_final, psychlops_q4_1_sessao_final), 
              na.rm = TRUE),
          TRUE ~ NA
        )
      ) |>
      select(record_id, psychlops_score_f) |>
      ungroup(),
    by = c("record_id")
  ) |>
  left_join(
    df |>
      select(record_id, redcap_event_name, encerramento_sesso_complete, contains("psychlops")) |>
      filter(
        redcap_event_name == "Seguimento 3m (Arm 1: Participantes)",
        record_id %in% seg_particip_3m_realizados_ids
      ) |>
      mutate(
        across(c(psychlops_q1_2_seg_3m, psychlops_q2_2_seg_3m,
                 psychlops_q3_2_seg_3m, psychlops_q4_1_seg_3m), 
               \(x) as.numeric(x) - 1)
      ) |>
      group_by(record_id) |>
      mutate(
        psychlops_score_3m = case_when(
          !is.na(psychlops_q1_2_seg_3m) & 
            is.na(psychlops_q2_2_seg_3m) ~ sum(
              c(psychlops_q1_2_seg_3m*2,
                psychlops_q3_2_seg_3m, psychlops_q4_1_seg_3m), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_seg_3m) & 
            !is.na(psychlops_q2_2_seg_3m) ~ sum(
              c(psychlops_q1_2_seg_3m, psychlops_q2_2_seg_3m,
                psychlops_q3_2_seg_3m, psychlops_q4_1_seg_3m), 
              na.rm = TRUE),
          TRUE ~ NA
        )
      ) |>
      select(record_id, psychlops_score_3m) |>
      ungroup(),
    by = c("record_id")
  ) |>
  left_join(
    df |>
      select(record_id, redcap_event_name, encerramento_sesso_complete, contains("psychlops")) |>
      filter(
        redcap_event_name == "Seguimento 6m (Arm 1: Participantes)",
        record_id %in% seg_particip_6m_realizados_ids
      ) |>
      mutate(
        across(c(psychlops_q1_2_seg_6m, psychlops_q2_2_seg_6m,
                 psychlops_q3_2_seg_6m, psychlops_q4_1_seg_6m), 
               \(x) as.numeric(x) - 1)
      ) |>
      group_by(record_id) |>
      mutate(
        psychlops_score_6m = case_when(
          !is.na(psychlops_q1_2_seg_6m) & 
            is.na(psychlops_q2_2_seg_6m) ~ sum(
              c(psychlops_q1_2_seg_6m*2,
                psychlops_q3_2_seg_6m, psychlops_q4_1_seg_6m), 
              na.rm = TRUE),
          !is.na(psychlops_q1_2_seg_6m) & 
            !is.na(psychlops_q2_2_seg_6m) ~ sum(
              c(psychlops_q1_2_seg_6m, psychlops_q2_2_seg_6m,
                psychlops_q3_2_seg_6m, psychlops_q4_1_seg_6m), 
              na.rm = TRUE),
          TRUE ~ NA
        )
      ) |>
      select(record_id, psychlops_score_6m) |>
      ungroup(),
    by = c("record_id")
  ) |>
  ungroup() 

## Long
psychlops_long_df <- psychlops_df |>
  pivot_longer(
    cols = psychlops_score_a:psychlops_score_6m,
    names_to = "sessao", values_to = "escore"
  ) |>
  distinct(record_id, sessao, escore) |>
  filter(!is.na(escore)) |>
  mutate(
    sessao = factor(case_when(
      sessao == "psychlops_score_a" ~ "Sessão A",
      sessao == "psychlops_score_1" ~ "Sessão 1",
      sessao == "psychlops_score_2" ~ "Sessão 2",
      sessao == "psychlops_score_3" ~ "Sessão 3",
      sessao == "psychlops_score_4" ~ "Sessão 4",
      sessao == "psychlops_score_5" ~ "Sessão 5",
      sessao == "psychlops_score_f" ~ "Sessão F",
      sessao == "psychlops_score_3m" ~ "3 meses",
      sessao == "psychlops_score_6m" ~ "6 meses",
      sessao == "psychlops_score_1_5" ~ "Sessões 1 a 5",
      TRUE ~ NA
    )),
    sessao = fct_relevel(
      sessao, "Sessão A", "Sessão 1", "Sessão 2", "Sessão 3",
      "Sessão 4", "Sessão 5", "Sessão F", "3 meses", "6 meses", 
      "Sessões 1 a 5")
  )


## Resumos ---------------------------------------------------------------------
### Todos
psychlops_long_summ_df <- psychlops_long_df |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE),
    n_ids = dplyr::n_distinct(record_id)
  )
psychlops_long_mean_df <- psychlops_long_df |>
  group_by(sessao) |>
  summarise(escore_media = round(mean(escore), 2))
### Completaram intervenção
psychlops_long_summ_completos_interv_df <- psychlops_long_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE),
    n_ids = dplyr::n_distinct(record_id)
  )
psychlops_long_mean_completos_interv_df <- psychlops_long_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  group_by(sessao) |>
  summarise(escore_media = round(mean(escore), 2))
### Completaram seguimento
psychlops_long_summ_completos_seg_df <- psychlops_long_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  group_by(sessao) |>
  summarise(
    escore_media = round(mean(escore, na.rm = TRUE), 2),
    escore_q1    = quantile(escore, .25, na.rm = TRUE),
    escore_q3    = quantile(escore, .75, na.rm = TRUE),
    n_ids = dplyr::n_distinct(record_id)
  )
psychlops_long_mean_completos_seg_df <- psychlops_long_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  group_by(sessao) |>
  summarise(escore_media = round(mean(escore), 2))


# N =========================================================================
psychlops_sa_n <- psychlops_df |>
  distinct(record_id, psychlops_score_a) |>
  filter(!is.na(psychlops_score_a)) |>
  nrow()

psychlops_s1_n <- psychlops_df |>
  distinct(record_id, psychlops_score_1) |>
  filter(!is.na(psychlops_score_1)) |>
  nrow()

psychlops_s2_n <- psychlops_df |>
  distinct(record_id, psychlops_score_2) |>
  filter(!is.na(psychlops_score_2)) |>
  nrow()

psychlops_s3_n <- psychlops_df |>
  distinct(record_id, psychlops_score_3) |>
  filter(!is.na(psychlops_score_3)) |>
  nrow()

psychlops_s4_n <- psychlops_df |>
  distinct(record_id, psychlops_score_4) |>
  filter(!is.na(psychlops_score_4)) |>
  nrow()

psychlops_s5_n <- psychlops_df |>
  distinct(record_id, psychlops_score_5) |>
  filter(!is.na(psychlops_score_5)) |>
  nrow()

psychlops_sf_n <- psychlops_df |>
  distinct(record_id, psychlops_score_f) |>
  filter(!is.na(psychlops_score_f)) |>
  nrow()

psychlops_3m_n <- psychlops_df |>
  distinct(record_id, psychlops_score_3m) |>
  filter(!is.na(psychlops_score_3m)) |>
  nrow()

psychlops_6m_n <- psychlops_df |>
  distinct(record_id, psychlops_score_6m) |>
  filter(!is.na(psychlops_score_6m)) |>
  nrow()


# STRINGS ======================================================================
## Completo -------------------------------------------------------
### Todos -------------------------------------------------
#### Sessão A
psychlops_sa_str <- psychlops_df |>
  distinct(record_id, psychlops_score_a) |> 
  summarise(
    media_sa  = mean(psychlops_score_a, na.rm = TRUE),
    median_sa = median(psychlops_score_a, na.rm = TRUE),
    sd_sa     = sd(psychlops_score_a, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_sa, 2)} ({round(sd_sa, 2)})")
  ) |> 
  pull(res_sa)
#### Sessões 1 a 5
psychlops_s1_s5_str <- psychlops_df |>
  select(record_id, psychlops_score_1_5) |>
  summarise(
    media_s1_s5  = mean(psychlops_score_1_5, na.rm = TRUE), 
    median_s1_s5 = median(psychlops_score_1_5, na.rm = TRUE), 
    sd_s1_s5     = sd(psychlops_score_1_5, na.rm = TRUE)
  ) |>
  reframe(
    res_s1_s5 = glue::glue("{round(media_s1_s5, 2)} ({round(sd_s1_s5,2)})")
  ) |>
  pull(res_s1_s5)
#### Sessão Final
psychlops_sf_str <- psychlops_df |>
  select(record_id, psychlops_score_f) |>
  summarise(
    media_sf  = mean(psychlops_score_f, na.rm = TRUE),
    median_sf = median(psychlops_score_f, na.rm = TRUE),
    sd_sf     = sd(psychlops_score_f, na.rm = TRUE)
  ) |>
  reframe(
    res_sf = glue::glue("{round(media_sf, 2)} ({round(sd_sf,2)})")
  ) |>
  pull(res_sf)
#### 3 meses
psychlops_3m_str <- psychlops_df |>
  select(record_id, psychlops_score_3m) |>
  summarise(
    media_3m  = mean(psychlops_score_3m, na.rm = TRUE),
    median_3m = median(psychlops_score_3m, na.rm = TRUE),
    sd_3m     = sd(psychlops_score_3m, na.rm = TRUE)
  ) |>
  reframe(
    res_3m = glue::glue("{round(media_3m, 2)} ({round(sd_3m,2)})")
  ) |>
  pull(res_3m)
#### 6 meses
psychlops_6m_str <- psychlops_df |>
  select(record_id, psychlops_score_6m) |>
  summarise(
    media_6m  = mean(psychlops_score_6m, na.rm = TRUE),
    median_6m = median(psychlops_score_6m, na.rm = TRUE),
    sd_6m     = sd(psychlops_score_6m, na.rm = TRUE)
  ) |>
  reframe(
    res_6m = glue::glue("{round(media_6m, 2)} ({round(sd_6m,2)})")
  ) |>
  pull(res_6m)


### Filtrado ------------------------------------------------------
#### Sessão A
psychlops_filtr_sa_str <- psychlops_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  select(record_id, psychlops_score_a) |>
  summarise(
    media_sa  = mean(psychlops_score_a, na.rm = TRUE),
    median_sa = median(psychlops_score_a, na.rm = TRUE),
    sd_sa     = sd(psychlops_score_a, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_sa, 2)} ({round(sd_sa, 2)})")
  ) |> 
  pull(res_sa)
#### Sessões 1 a 5
psychlops_filtr_s1_s5_str <- psychlops_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_s1_s5  = mean(psychlops_score_1_5, na.rm = TRUE), 
    median_s1_s5 = median(psychlops_score_1_5, na.rm = TRUE), 
    sd_s1_s5     = sd(psychlops_score_1_5, na.rm = TRUE)
  ) |>
  reframe(
    res_s1_s5 = glue::glue("{round(media_s1_s5, 2)} ({round(sd_s1_s5,2)})")
  ) |>
  pull(res_s1_s5)
#### Sessão Final
psychlops_filtr_sf_str <- psychlops_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_sf  = mean(psychlops_score_f, na.rm = TRUE),
    median_sf = median(psychlops_score_f, na.rm = TRUE),
    sd_sf     = sd(psychlops_score_f, na.rm = TRUE)
  ) |>
  reframe(
    res_sf = glue::glue("{round(media_sf, 2)} ({round(sd_sf,2)})")
  ) |>
  pull(res_sf)
#### 3 meses
psychlops_filtr_3m_str <- psychlops_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_3m  = mean(psychlops_score_3m, na.rm = TRUE),
    median_3m = median(psychlops_score_3m, na.rm = TRUE),
    sd_3m     = sd(psychlops_score_3m, na.rm = TRUE)
  ) |>
  reframe(
    res_3m = glue::glue("{round(media_3m, 2)} ({round(sd_3m,2)})")
  ) |>
  pull(res_3m)
#### 6 meses
psychlops_filtr_6m_str <- psychlops_df |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  summarise(
    media_6m  = mean(psychlops_score_6m, na.rm = TRUE),
    median_6m = median(psychlops_score_6m, na.rm = TRUE),
    sd_6m     = sd(psychlops_score_6m, na.rm = TRUE)
  ) |>
  reframe(
    res_6m = glue::glue("{round(media_6m, 2)} ({round(sd_6m,2)})")
  ) |>
  pull(res_6m)

## Intervenção -------------------------------------------------
#### Sessão A
psychlops_filtr_interv_sa_str <- psychlops_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  select(record_id, psychlops_score_a) |>
  summarise(
    media_sa  = mean(psychlops_score_a, na.rm = TRUE),
    median_sa = median(psychlops_score_a, na.rm = TRUE),
    sd_sa     = sd(psychlops_score_a, na.rm = TRUE)
  ) |>
  reframe(
    res_sa = glue::glue("{round(media_sa, 2)} ({round(sd_sa, 2)})")
  ) |> 
  pull(res_sa)
#### Sessões 1 a 5
psychlops_filtr_interv_s1_s5_str <- psychlops_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_s1_s5  = mean(psychlops_score_1_5, na.rm = TRUE), 
    median_s1_s5 = median(psychlops_score_1_5, na.rm = TRUE), 
    sd_s1_s5     = sd(psychlops_score_1_5, na.rm = TRUE)
  ) |>
  reframe(
    res_s1_s5 = glue::glue("{round(media_s1_s5, 2)} ({round(sd_s1_s5,2)})")
  ) |>
  pull(res_s1_s5)
#### Sessão Final
psychlops_filtr_interv_sf_str <- psychlops_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_sf  = mean(psychlops_score_f, na.rm = TRUE),
    median_sf = median(psychlops_score_f, na.rm = TRUE),
    sd_sf     = sd(psychlops_score_f, na.rm = TRUE)
  ) |>
  reframe(
    res_sf = glue::glue("{round(media_sf, 2)} ({round(sd_sf,2)})")
  ) |>
  pull(res_sf)
#### 3 meses
psychlops_filtr_interv_3m_str <- psychlops_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_3m  = mean(psychlops_score_3m, na.rm = TRUE),
    median_3m = median(psychlops_score_3m, na.rm = TRUE),
    sd_3m     = sd(psychlops_score_3m, na.rm = TRUE)
  ) |>
  reframe(
    res_3m = glue::glue("{round(media_3m, 2)} ({round(sd_3m,2)})")
  ) |>
  pull(res_3m)
#### 6 meses
psychlops_filtr_interv_6m_str <- psychlops_df |>
  filter(record_id %in% interv_sf_realiz_ids) |>
  summarise(
    media_6m  = mean(psychlops_score_6m, na.rm = TRUE),
    median_6m = median(psychlops_score_6m, na.rm = TRUE),
    sd_6m     = sd(psychlops_score_6m, na.rm = TRUE)
  ) |>
  reframe(
    res_6m = glue::glue("{round(media_6m, 2)} ({round(sd_6m,2)})")
  ) |>
  pull(res_6m)

# EFFECT SIZE =================================================================
## Completo ------------------------------------------------------
# Baseado en https://pmc.ncbi.nlm.nih.gov/articles/PMC3840331/
psychlops_effect_size <- psychlops_df |>
  filter(!is.na(psychlops_score_a)) |>
  distinct(record_id, psychlops_score_a, psychlops_score_f) |>
  filter(!is.na(psychlops_score_f)) |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  mutate(
    score_diff = psychlops_score_a - psychlops_score_f
  ) |>
  summarise(
    mean_diff  = mean(score_diff),
    sd_diff    = sd(score_diff),
    cohen_dz   = round((mean_diff - 0)/(sd_diff), 2)
  ) |>
  pull(cohen_dz)
# Baseado na solicitação de comparação a nível do indivíduo
psychlops_effect_size2 <- psychlops_df |>
  filter(!is.na(psychlops_score_a)) |>
  distinct(record_id, psychlops_score_a, psychlops_score_f) |>
  filter(!is.na(psychlops_score_f)) |>
  filter(record_id %in% seg_particip_6m_realizados_ids) |>
  mutate(
    score_diff  = psychlops_score_a - psychlops_score_f,
    sd_sa       = sd(psychlops_score_a),
    effect_size = score_diff/sd_sa
  ) |>
  summarise(
    effect_size_mean = round(mean(effect_size), 2)
  ) |>
  pull()


## Intervenção ------------------------------------------------------
# Baseado en https://pmc.ncbi.nlm.nih.gov/articles/PMC3840331/
psychlops_interv_effect_size <- psychlops_df |>
  filter(!is.na(psychlops_score_a)) |>
  distinct(record_id, psychlops_score_a, psychlops_score_f) |>
  filter(!is.na(psychlops_score_f)) |>
  mutate(
    score_diff = psychlops_score_a - psychlops_score_f
  ) |>
  summarise(
    mean_diff  = mean(score_diff),
    sd_diff    = sd(score_diff),
    cohen_dz   = round((mean_diff - 0)/(sd_diff), 2)
  ) |>
  pull(cohen_dz)
# Baseado na solicitação de comparação a nível do indivíduo
psychlops_interv_effect_size2 <- psychlops_df |>
  filter(!is.na(psychlops_score_a)) |>
  distinct(record_id, psychlops_score_a, psychlops_score_f) |>
  filter(!is.na(psychlops_score_f)) |>
  mutate(
    score_diff  = psychlops_score_a - psychlops_score_f,
    sd_sa       = sd(psychlops_score_a),
    effect_size = score_diff/sd_sa
  ) |>
  summarise(
    effect_size_mean = round(mean(effect_size), 2)
  ) |>
  pull()


# TABELAS =====================================================================
## Completo -----------------------------------------------------
tabela_psychlops <- tibble(
  `Sessão A`      = psychlops_sa_str,
  `Sessões 1 a 5` = psychlops_s1_s5_str,
  `Sessão F`      = psychlops_sf_str,
  `3 meses`       = psychlops_3m_str,
  `6 meses`       = psychlops_6m_str
)
tabela_psychlops_filtr <- tibble(
  `Sessão A`      = psychlops_filtr_sa_str,
  `Sessões 1 a 5` = psychlops_filtr_s1_s5_str,
  `Sessão F`      = psychlops_filtr_sf_str,
  `3 meses`       = psychlops_filtr_3m_str,
  `6 meses`       = psychlops_filtr_6m_str
) |>
  mutate(
    `Cohen's dz`  = psychlops_effect_size,
    `Effect Size` = psychlops_effect_size2
  )

## Intervenção ------------------------------------------------
tabela_psychlops_filtr_interv <- tibble(
  `Sessão A`      = psychlops_filtr_interv_sa_str,
  `Sessões 1 a 5` = psychlops_filtr_interv_s1_s5_str,
  `Sessão F`      = psychlops_filtr_interv_sf_str,
  `3 meses`       = psychlops_filtr_interv_3m_str,
  `6 meses`       = psychlops_filtr_interv_6m_str
) |>
  mutate(
    `Cohen's dz`  = psychlops_interv_effect_size,
    `Effect Size` = psychlops_interv_effect_size2
  )



# TEXTO =======================================================================
texto_psychlops <- paste0(
  "<b>O escore é calculado da seguinte maneira para todas as Sessões:</b> <br>
<b>Se Questão 2b foi preenchida:</b> Q1 + Q2 + Q3 + Q4; <br>
<b>Se Questão 2b NÃO foi preenchida:</b> (Q1 x 2) + Q3 + Q4. <br>
<br>
Nas Sessões 1 a 5, para cada participante, foi primeiro calculado o escore em cada Sessão, e então a média dentre as Sessões foi calculada. <br> 
Escore descrito como <b>Média (Desvio Padrão)</b><br>
<br>
<b>Effect Size</b> foi calculado como média de todos os efeitos, calculados para cada participante por (SA - SF)/(Desvio Padrão SA).<br>
<br>
<b>Cohen's dz</b> foi calculado como (Média da Diferença)/(DP da Diferença).<br>
Referência: https://doi.org/10.3389/fpsyg.2013.00863 (eq. 6)."
)