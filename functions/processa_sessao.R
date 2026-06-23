processa_sessao <- function(df, ids, event, prefixo) {
  df |>
    filter(record_id %in% ids, redcap_event_name == event) |>
    select(record_id, enc_sessao_agend_data, 
           encerramento_sesso_complete, checklist_de_sesso_complete,
           desc_dta_sessao, data_reagend_sessao_1_1, data_reagend_sessao_1_2, data_reagend_sessao_1_3,
           desist_motivo_sessao_1, desist_motivo_reagend_sessao_1_2, desist_motivo_reagend_sessao_1_3
    ) |>
    group_by(record_id) |>
    mutate(
      # "{prefixo}_realizada"          := as.integer(!is.na(enc_sessao_agend_data)),
      "{prefixo}_realizada"          := as.integer(encerramento_sesso_complete == "Complete" |
                                                     checklist_de_sesso_complete == "Complete"),
      across(c(desc_dta_sessao, data_reagend_sessao_1_1, data_reagend_sessao_1_2, data_reagend_sessao_1_3),
             \(x) as.character(as.Date(x))),
      "{prefixo}_data"               := case_when(
        data[[paste0(prefixo, "_realizada")]] == 1L ~ max(
          c(desc_dta_sessao, data_reagend_sessao_1_1, data_reagend_sessao_1_2, data_reagend_sessao_1_3),
          na.rm = TRUE
        ),
        data[[paste0(prefixo, "_realizada")]] == 0L ~ NA
      ),
      "motivo_desistencia_{prefixo}" := coalesce(desist_motivo_sessao_1, desist_motivo_reagend_sessao_1_2, desist_motivo_reagend_sessao_1_3),
      "desistencia_{prefixo}"        := as.integer(!is.na(.data[[paste0("motivo_desistencia_", prefixo)]]))
    ) |>
    ungroup() |>
    select(record_id, contains(prefixo))
}