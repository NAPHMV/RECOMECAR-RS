interv_aguardando_sessao <- function(sessao) {
  if (sessao == 1) {
    df_filtrado <- df |>
      filter(
        redcap_event_name == "Sessao 1 (Arm 1: Participantes)" &
        (is.na(enc_sessao_agend_data) | is.na(enc_sessao_data)),
        # (is.na(enc_sessao_agend_data) & is.na(enc_sessao_data) &
        #   !encerramento_sesso_complete %in% "Complete" &
        #   !checklist_de_sesso_complete %in% "Complete") &
        !record_id %in% interv_s1_aguard_agend_ids
      ) |>
      inner_join(
        interv_andamento_df |>
          filter(
            sessao_A_realizada == 1,
            sessao_1_realizada == 0
          ) |>
          select(record_id),
        by = "record_id"
      )
  } else if (sessao == 2) {
    df_filtrado <- df |>
      filter(
        redcap_event_name == "Sessao 2 (Arm 1: Participantes)",
        is.na(enc_sessao_agend_data),
        !record_id %in% interv_s2_aguard_agend_ids
      ) |>
      inner_join(
        interv_andamento_df |>
          filter(
            sessao_1_realizada == 1,
            sessao_2_realizada == 0
          ) |>
          select(record_id),
        by = "record_id"
      )
  } else if (sessao == 3) {
    df_filtrado <- df |>
      filter(
        redcap_event_name == "Sessao 3 (Arm 1: Participantes)",
        is.na(enc_sessao_agend_data),
        !record_id %in% interv_s3_aguard_agend_ids
      ) |>
      inner_join(
        interv_andamento_df |>
          filter(
            sessao_2_realizada == 1,
            sessao_3_realizada == 0
          ) |>
          select(record_id),
        by = "record_id"
      )
  } else if (sessao == 4) {
    df_filtrado <- df |>
      filter(
        redcap_event_name == "Sessao 4 (Arm 1: Participantes)",
        is.na(enc_sessao_agend_data),
        !record_id %in% interv_s4_aguard_agend_ids
      ) |>
      inner_join(
        interv_andamento_df |>
          filter(
            sessao_3_realizada == 1,
            sessao_4_realizada == 0
          ) |>
          select(record_id),
        by = "record_id"
      )
  } else if (sessao == 5) {
    df_filtrado <- df |>
      filter(
        redcap_event_name == "Sessao 5 (Arm 1: Participantes)",
        is.na(enc_sessao_agend_data),
        !record_id %in% interv_s5_aguard_agend_ids
      ) |>
      inner_join(
        interv_andamento_df |>
          filter(
            sessao_4_realizada == 0,
            sessao_5_realizada == 0
          ) |>
          select(record_id),
        by = "record_id"
      )
  } else if (sessao == "final") {
    df_filtrado <- df |>
      filter(
        redcap_event_name == "Sessao final (Arm 1: Participantes)",
        is.na(enc_sessao_agend_data),
        !record_id %in% interv_sf_aguard_agend_ids
      ) |>
      inner_join(
        interv_andamento_df |>
          filter(
            sessao_5_realizada == 1,
            sessao_final_realizada == 0
          ) |>
          select(record_id),
        by = "record_id"
      )
  }
  
  df_filtrado |>
    anti_join(
      df |>
        filter(
          redcap_event_name == "Desfecho (Arm 1: Participantes)",
          desfecho_participante_interv == "Retirado"
        ) |>
        select(record_id)
    ) |>
    group_by(record_id) |>
    mutate(
      ultima_tentativa_agend = case_when(
        (tentativa_agendar_sessao_1 %in% "Sim" &
           !tentativa_agendar_sessao_2 %in% "Sim" &
           !tentativa_agendar_sessao_3 %in% "Sim" &
           !tentativa_agendar_sessao_4 %in% "Sim" &
           !tentativa_agendar_sessao_5 %in% "Sim" &
           !tentativa_agendar_sessao_6 %in% "Sim") ~ 1,
        (tentativa_agendar_sessao_2 %in% "Sim" &
           !tentativa_agendar_sessao_3 %in% "Sim" &
           !tentativa_agendar_sessao_4 %in% "Sim" &
           !tentativa_agendar_sessao_5 %in% "Sim" &
           !tentativa_agendar_sessao_6 %in% "Sim") ~ 2,
        (tentativa_agendar_sessao_3 %in% "Sim" &
           !tentativa_agendar_sessao_4 %in% "Sim" &
           !tentativa_agendar_sessao_5 %in% "Sim" &
           !tentativa_agendar_sessao_6 %in% "Sim") ~ 3,
        (tentativa_agendar_sessao_4 %in% "Sim" &
           !tentativa_agendar_sessao_5 %in% "Sim" &
           !tentativa_agendar_sessao_6 %in% "Sim") ~ 4,
        (tentativa_agendar_sessao_5 %in% "Sim" &
           !tentativa_agendar_sessao_6 %in% "Sim") ~ 5,
        tentativa_agendar_sessao_6 %in% "Sim" ~ 6,
        TRUE ~ NA
      )
    ) |>
    ungroup() |>
    filter(
      # !record_id %in% interv_sa_exclusao_ids,
      # !record_id %in% interv_sa_nao_concordaram_ids
      # tentativa_agendar_sessao_1 == "Sim"
      !is.na(ultima_tentativa_agend)
    ) |>
    slice_max(ultima_tentativa_agend, by = record_id) |>
    distinct(record_id) |>
    pull()
}