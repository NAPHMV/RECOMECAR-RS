# Variáveis para checar ======================================================
# unique(data_dictionary$form_name)

ausentes_interv_s2_vars <- data_dictionary |>
  filter(field_name %in% c(
    # Acompanhamento ----------------------------------------------------
    "desc_dta_sessao", 'cartao_valid', 'preencheu_anterior',
    # Pré-Sessão -------------------------------------------------------
    'escalas_auto_gravacao_confirma', #'escalas_auto_dta',
    # GAD-7 ------------------------------------------------------------
    'gad7_perg_1', 'gad7_perg_2', 'gad7_perg_3', 'gad7_perg4', 'gad7_perg_5',
    'gad7_perg_6', 'gad7_perg_7', 'score_gad_7', 'classific_gad_7',
    # PHQ-9 ------------------------------------------------------------
    'phq9_dta_preenchi', 'phq9_perg_1', 'phq9_perg_2', 'phq9_perg_3',
    'phq9_perg_4', 'phq9_perg_5', 'phq9_perg_6', 'phq9_perg_7', 'phq9_perg_8',
    'phq9_perg_9', 'score_phq_9', 'classific_phq_9',
    # PSYCHLOPS -------------------------------------------------------
    'psychlops_q1_2_sessao_2', 'psychlops_q2_2_sessao_2', 'psychlops_q3_2_sessao_2', 
    'psychlops_q4_1_sessao_2', 'psychlops_suic_1_sessao_2', 'psychlops_suic_1_1_sessao_2', 
    'psychlops_suic_2_sessao_2', 'psychlops_suic_3_sessao_2', 'psychlops_suic_3_1_sessao_2',
    #'psychlops_q5_2_sessao_2',
    # Encerramento -----------------------------------------------------
    'enc_sessao_superv', 'enc_sessao_superv_obs', 'enc_sessao_superv_apto', 
    'enc_sessao_atend_ind', 'enc_sessao_atend_ind_ql___1',
    'enc_sessao_atend_ind_ql___2', 'enc_sessao_atend_ind_ql___3',
    'enc_sessao_atend_ind_ql___4'
  )) |>
  pull(field_name)

ausentes_interv_s2_condicoes <- list(
  cartao_valid = cond_eq('cartao_ressarc', 'Sim'),
  psychlops_suic_1_1_sessao_2 = cond_eq('psychlops_suic_1_sessao_2', 'Sim'),
  psychlops_suic_2_sessao_2 = cond_eq('psychlops_suic_1_sessao_2', 'Sim'),
  psychlops_suic_3_sessao_2 = cond_eq('psychlops_suic_1_sessao_2', 'Sim'),
  psychlops_suic_3_1_sessao_2 = cond_in('psychlops_suic_3_sessao_2',
                                        c('Sim', 'Não tenho certeza')),
  enc_sessao_superv_obs = cond_eq('enc_sessao_superv', 'Sim'),
  enc_sessao_superv_apto = cond_eq('enc_sessao_superv', 'Sim'),
  enc_sessao_atend_ind_ql___1 = cond_eq('enc_sessao_atend_ind', 'Sim'),
  enc_sessao_atend_ind_ql___2 = cond_eq('enc_sessao_atend_ind', 'Sim'),
  enc_sessao_atend_ind_ql___3 = cond_eq('enc_sessao_atend_ind', 'Sim'),
  enc_sessao_atend_ind_ql___4 = cond_eq('enc_sessao_atend_ind', 'Sim')
)

ausentes_interv_s2_escopo <- c(
  setNames(rep(list(interv_s2_realiz_ids), 
               length(ausentes_interv_s2_vars)), 
           ausentes_interv_s2_vars)
)


# checagem de sanidade
stopifnot(!any(duplicated(names(ausentes_interv_s2_escopo))))

ausentes_interv_s2_df <- checar_faltantes(
  df |>
    filter(
      redcap_event_name == "Sessao 2 (Arm 1: Participantes)" &
        record_id %in% interv_s2_realiz_ids), 
  id_col = "record_id", 
  vars = ausentes_interv_s2_vars, 
  condicoes = ausentes_interv_s2_condicoes)