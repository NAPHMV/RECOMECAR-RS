####====================== Pré-Triagem =====================================###
# Iniciadas ===========================================================
pretri_iniciada_ids <- df |>
  filter(redcap_event_name == 'Triagem (Arm 1: Participantes)') %>% 
  distinct(record_id) |>
  pull()
pretri_iniciada_n <- length(pretri_iniciada_ids)

# Realizadas ==========================================================
pretri_realiz_ids <- df |>
  filter(
    !is.na(sair_casa_enchente) | 
      !is.na(perda_material) |
      !is.na(ilhado)
  ) |>
  distinct(record_id) |>
  pull()
pretri_realiz_n <- length(pretri_realiz_ids)


# Elegíveis p/ Triagem ==============================================
pretri_eleg_tri_ids <- df |>
  filter(calc_elegi_pre_triagem == 1) |>
  distinct(record_id) |>
  pull()
pretri_eleg_tri_n <- length(pretri_eleg_tri_ids)
