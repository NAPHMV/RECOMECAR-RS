interv_aguardando_agendamento <- function(sessao) {
  if (sessao == 1) {
    dados_andamento <- interv_andamento_df |>
      filter(
        sessao_A_realizada == 1 &
          (sessao_1_realizada == 0 | is.na(sessao_1_realizada))
        # if_all(starts_with("desistencia_sessao_"), \(x) x == 0)
      ) |>
      select(record_id) 
  } else if (sessao == 2) {
    dados_andamento <- interv_andamento_df |>
      filter(
        sessao_1_realizada == 1 &
          (sessao_2_realizada == 0 | is.na(sessao_2_realizada))
        # if_all(starts_with("desistencia_sessao_"), \(x) x == 0)
      ) |>
      select(record_id) 
  } else if (sessao == 3) {
    dados_andamento <- interv_andamento_df |>
      filter(
        sessao_2_realizada == 1 &
          (sessao_3_realizada == 0 | is.na(sessao_3_realizada))
        # if_all(starts_with("desistencia_sessao_"), \(x) x == 0)
      ) |>
      select(record_id) 
  } else if (sessao == 4) {
    dados_andamento <- interv_andamento_df |>
      filter(
        sessao_3_realizada == 1,
        (sessao_4_realizada == 0 | is.na(sessao_4_realizada))
        # if_all(starts_with("desistencia_sessao_"), \(x) x == 0)
      ) |>
      select(record_id) 
  } else if (sessao == 5) {
    dados_andamento <- interv_andamento_df |>
      filter(
        sessao_4_realizada == 1 &
          (sessao_5_realizada == 0 | is.na(sessao_5_realizada))
        # if_all(starts_with("desistencia_sessao_"), \(x) x == 0)
      ) |>
      select(record_id) 
  } else if (sessao == "final") {
    dados_andamento <- interv_andamento_df |>
      filter(
        sessao_5_realizada == 1,
        (sessao_final_realizada == 0 | is.na(sessao_final_realizada))
        # if_all(starts_with("desistencia_sessao_"), \(x) x == 0)
      ) |>
      select(record_id) 
  }
  
  # reagend_cols <- paste0("data_reagend_sessao_1_", 1:3)
  # 
  # df_filtrado <- df %>%
  #   filter(redcap_event_name == glue::glue("Sessao {sessao} (Arm 1: Participantes)"))
  # 
  # resultado <- purrr::pmap_dfr(df_filtrado[reagend_cols], function(...) {
  #   x <- c(...)
  #   idx <- which(!is.na(x))
  #   if (length(idx) == 0) {
  #     tibble::tibble(ultima_reagend = as.Date(NA), ultima_reagend_num = NA_integer_)
  #   } else {
  #     ultimo_idx <- max(idx)
  #     tibble::tibble(ultima_reagend = as.Date(x[ultimo_idx]), ultima_reagend_num = ultimo_idx)
  #   }
  # })
  # 
  # ultima_reagend <- df_filtrado %>%
  #   bind_cols(resultado) %>%
  #   select(record_id, ultima_reagend, ultima_reagend_num)
  
  # dados_agendamento <- df |>
  #   filter(
  #     redcap_event_name == glue::glue("Sessao {sessao} (Arm 1: Participantes)")
  #   ) |>
  #   mutate(
  #     ultimo_contato = case_when(
  #       (tentativa_contato_realiz_1 %in% "Sim" &
  #          !tentativa_contato_realiz_2 %in% "Sim" &
  #          !tentativa_contato_realiz_3 %in% "Sim" &
  #          !tentativa_contato_realiz_4 %in% "Sim" &
  #          !tentativa_contato_realiz_5 %in% "Sim" &
  #          !tentativa_contato_realiz_6 %in% "Sim") ~ 1,
  #       (tentativa_contato_realiz_2 %in% "Sim" &
  #          !tentativa_contato_realiz_3 %in% "Sim" &
  #          !tentativa_contato_realiz_4 %in% "Sim" &
  #          !tentativa_contato_realiz_5 %in% "Sim" &
  #          !tentativa_contato_realiz_6 %in% "Sim") ~ 2,
  #       (tentativa_contato_realiz_3 %in% "Sim" &
  #          !tentativa_contato_realiz_4 %in% "Sim" &
  #          !tentativa_contato_realiz_5 %in% "Sim" &
  #          !tentativa_contato_realiz_6 %in% "Sim") ~ 3,
  #       (tentativa_contato_realiz_4 %in% "Sim" &
  #          !tentativa_contato_realiz_5 %in% "Sim" &
  #          !tentativa_contato_realiz_6 %in% "Sim") ~ 4,
  #       (tentativa_contato_realiz_5 %in% "Sim" &
  #          !tentativa_contato_realiz_6 %in% "Sim") ~ 5,
  #       tentativa_contato_realiz_6 %in% "Sim" ~ 6,
  #       TRUE ~ NA
  #     )
  #   ) |>
  #   slice_max(ultimo_contato, by = "record_id", na.rm = FALSE) |>
  #   filter(!if_any(c(tentativa_agendar_sessao_1, tentativa_agendar_sessao_2,
  #                        tentativa_agendar_sessao_3, tentativa_agendar_sessao_4,
  #                        tentativa_agendar_sessao_5, tentativa_agendar_sessao_6),
  #          \(x) x %in% "Sim"))
  
  df_ids <- df |>
    filter(
      redcap_event_name == glue::glue("Sessao {sessao} (Arm 1: Participantes)"),
      is.na(tentativa_dta_agend_1),
      !record_id %in% (interv_andamento_df |>
                         select(
                           record_id, contains(glue::glue("sessao_{sessao}_realizada"))) |>
                         filter(if_any(everything(), \(x) x == 1)))
    ) |>
    full_join(
      dados_andamento,
      by = "record_id"
    ) |>
    anti_join(
      df |>
        filter(
          redcap_event_name == "Desfecho (Arm 1: Participantes)",
          desfecho_participante_interv == "Retirado"
        ) |>
        select(record_id),
      by = "record_id"
    ) |>
    # group_by(record_id) |>
    # mutate(
    #   ultima_tentativa_agend = case_when(
    #     (tentativa_agendar_sessao_1 %in% "Sim" &
    #        !tentativa_agendar_sessao_2 %in% "Sim" &
    #        !tentativa_agendar_sessao_3 %in% "Sim" &
    #        !tentativa_agendar_sessao_4 %in% "Sim" &
    #        !tentativa_agendar_sessao_5 %in% "Sim" &
    #        !tentativa_agendar_sessao_6 %in% "Sim") ~ 1,
    #     (tentativa_agendar_sessao_2 %in% "Sim" &
    #        !tentativa_agendar_sessao_3 %in% "Sim" &
    #        !tentativa_agendar_sessao_4 %in% "Sim" &
    #        !tentativa_agendar_sessao_5 %in% "Sim" &
    #        !tentativa_agendar_sessao_6 %in% "Sim") ~ 2,
    #     (tentativa_agendar_sessao_3 %in% "Sim" &
    #        !tentativa_agendar_sessao_4 %in% "Sim" &
    #        !tentativa_agendar_sessao_5 %in% "Sim" &
    #        !tentativa_agendar_sessao_6 %in% "Sim") ~ 3,
    #     (tentativa_agendar_sessao_4 %in% "Sim" &
    #        !tentativa_agendar_sessao_5 %in% "Sim" &
    #        !tentativa_agendar_sessao_6 %in% "Sim") ~ 4,
    #     (tentativa_agendar_sessao_5 %in% "Sim" &
    #        !tentativa_agendar_sessao_6 %in% "Sim") ~ 5,
    #     tentativa_agendar_sessao_6 %in% "Sim" ~ 6,
    #     TRUE ~ NA
    #   )
    # ) |>
    # ungroup() |>
    # filter(
    #   # !record_id %in% interv_sa_exclusao_ids,
    #   # !record_id %in% interv_sa_nao_concordaram_ids
    #   # tentativa_agendar_sessao_1 == "Sim"
    #   !is.na(ultima_tentativa_agend)
    # ) |>
    # slice_max(ultima_tentativa_agend, by = record_id) |>
    distinct(record_id) |>
    pull()
  
  
  out_n <- length(df_ids)
  
  output <- list(ids = df_ids, n = out_n)
  return(df_ids)
  
}