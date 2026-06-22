# Iniciadas ========================================
tri_iniciada_n <- df %>% 
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  nrow()

# Finalizaram =====================================
# iniciaram e finalizaram a triagem
tri_realiz_ids <- df |>
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  filter(!is.na(calc_elegi_triagem)) |>
  pull(record_id)

tri_realiz_n <- length(tri_realiz_ids)

# Não finalizada ===================================
# iniciaram e não finalizaram a triagem
tri_nao_realiz_n <- df %>% 
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  filter(is.na(calc_elegi_triagem)) %>% 
  nrow()


# Sintomas ==========================================
tri_sintomas_n <- df %>% 
  filter(
    redcap_event_name == 'Triagem (Arm 1: Participantes)',
    record_id %in% tri_realiz_ids
  ) %>% 
  summarise(
    com_sintomas = sum(calc_elegi_triagem == 1 | calc_elegi_triagem == 2, na.rm = TRUE)
  ) |>
  pull(com_sintomas)

tri_sintomas_sem_n <- df |>
  filter(
    redcap_event_name == 'Triagem (Arm 1: Participantes)',
    record_id %in% tri_realiz_ids
  ) %>% 
  summarise(
    sem_sintomas = sum(calc_elegi_triagem == 0, na.rm = TRUE)
  ) |>
  pull(sem_sintomas)


# Elegíveis Intervenção ===================================
tri_eleg_interv_ids <- df %>% 
  filter(
    redcap_event_name == 'Triagem (Arm 1: Participantes)',
    record_id %in% tri_realiz_ids
  ) %>% 
  mutate(
    partic_elegivel = case_when(
      (calc_elegi_triagem == 1  & 
         aceita_particip != 'Não quero participar e não quero receber o contato da equipe') |
        ((particip_eleg_continuidade == 'Sim' |
            particip_eleg_continuidade_1 == 'Sim' |
            particip_eleg_continuidade_2 == 'Sim' | 
            particip_eleg_continuidade_3 == 'Sim') & 
           aceita_particip_2 !=
           'Não quero receber o contato da equipe do projeto') |
        atend_psiq_prosseg == "Sim" | atend_assist_encam_prosseg  == "Sim" ~ 1,
      TRUE ~ 0
    )
  ) |>
  filter(partic_elegivel == 1) |>
  distinct(record_id) |>
  pull()

tri_eleg_interv_n <- length(tri_eleg_interv_ids)

# Exclusões ==============================================
tri_nao_aceitaram_n <- df |>
  summarise(
    triagem_nao_aceite = sum(aceita_particip == 'Não quero participar e não quero receber o contato da equipe', na.rm = TRUE)
  ) |>
  pull(triagem_nao_aceite)

tri_exclusoes <- tibble(
  motivo = "Não aceitaram", n = tri_nao_aceitaram_n
)
tri_exclusoes_n <- sum(tri_exclusoes$n, na.rm = TRUE)

tri_exclusoes_str <- tri_exclusoes |>
  filter(n > 0) |>
  mutate(linha = glue("{motivo} = {n}")) |>
  pull(linha) |>
  paste(collapse = "\n")


# Manejo ======================================================================
## Elegíveis ======================================================
tri_manejo_eleg_ids <- df |>
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  filter(
    particip_eleg_continuidade_3 %in% "Sim" |
      particip_eleg_continuidade_2 %in% "Sim" |
      particip_eleg_continuidade %in% "Sim"
  ) |>
  distinct(record_id) |>
  pull()

tri_manejo_eleg_n <- length(tri_manejo_eleg_ids)



## Não elegíveis ==================================================
tri_manejo_nao_eleg_ids <- df |>
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  filter(
    particip_eleg_continuidade_3 %in% "Não" |
      particip_eleg_continuidade_2 %in% "Não" |
      particip_eleg_continuidade %in% "Não" 
  ) |>
  distinct(record_id) |>
  pull()

tri_manejo_nao_eleg_n <- length(tri_manejo_nao_eleg_ids)




# Meta triagens ================================================================
meta_triagens <- 10000
prop_meta_triagens <- paste0(round(100*(tri_realiz_n / meta_triagens), 2),"%")



