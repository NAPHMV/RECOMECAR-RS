# Variáveis para checar ======================================================
# unique(data_dictionary$form_name)

ausentes_interv_sf_vars <- data_dictionary |>
  filter(field_name %in% c(
    # Acompanhamento ----------------------------------------------------
    "desc_dta_sessao", 'cartao_valid', 'preencheu_anterior',
    # Pré-Sessão -------------------------------------------------------
    'escalas_auto_gravacao_confirma', #'escalas_auto_dta',
    # WHODAS -----------------------------------------------------------
    'whodas_q1', 'whodas_q2', 'whodas_q3', 'whodas_q4', 'whodas_q5', 'whodas_q6',
    'whodas_q7', "whodas_q8", 'whodas_q9', 'whodas_q10', 'whodas_q11', 'whodas_q12', 'whodas_score',
    # GAD-7 ------------------------------------------------------------
    'gad7_perg_1', 'gad7_perg_2', 'gad7_perg_3', 'gad7_perg4', 'gad7_perg_5',
    'gad7_perg_6', 'gad7_perg_7', 'score_gad_7', 'classific_gad_7',
    # PHQ-9 ------------------------------------------------------------
    'phq9_dta_preenchi', 'phq9_perg_1', 'phq9_perg_2', 'phq9_perg_3',
    'phq9_perg_4', 'phq9_perg_5', 'phq9_perg_6', 'phq9_perg_7', 'phq9_perg_8',
    'phq9_perg_9', 'score_phq_9', 'classific_phq_9',
    # PCL-5 ------------------------------------------------------------
    'pcl5_perg_1', 'pcl5_perg_2', 'pcl5_perg_3', 'pcl5_perg_4', 'pcl5_perg_5',
    'pcl5_perg_6', 'pcl5_perg_7', 'pcl5_perg_8', 'pcl5_perg_9', 'pcl5_perg_10',
    'pcl5_perg_11', 'pcl5_perg_12', 'pcl5_perg_13', 'pcl5_perg_14', 'pcl5_perg_15',   
    'pcl5_perg_16', 'pcl5_perg_17', 'pcl5_perg_18', 'pcl5_perg_19', 'pcl5_perg_20',
    'score_pcl_5', 'classific_pcl_5',
    # CCAS ------------------------------------------------------------
    'esc_clima_1', 'esc_clima_2', 'esc_clima_3', 'esc_clima_4', 'esc_clima_5',
    'esc_clima_6', 'esc_clima_7', 'esc_clima_8', 'esc_clima_9', 'esc_clima_10',
    'esc_clima_11', 'esc_clima_12', 'esc_clima_13', 'score_esc_clima',
    # PSYCHLOPS -------------------------------------------------------
    'psychlops_q1_2_sessao_final', 'psychlops_q2_2_sessao_final', 'psychlops_q3_2_sessao_final', 
    'psychlops_q4_1_sessao_final', 'psychlops_suic_1_sessao_final', 'psychlops_suic_1_1_sessao_final', 
    'psychlops_suic_2_sessao_final', 'psychlops_suic_3_sessao_final', 'psychlops_suic_3_1_sessao_final',
    #'psychlops_q5_2_sessao_final',
    # Encerramento -----------------------------------------------------
    'enc_sessao_superv', 'enc_sessao_superv_obs', 'enc_sessao_superv_apto', 
    'enc_sessao_atend_ind', 'enc_sessao_atend_ind_ql___1',
    'enc_sessao_atend_ind_ql___2', 'enc_sessao_atend_ind_ql___3',
    'enc_sessao_atend_ind_ql___4'
  )) |>
  pull(field_name)

ausentes_interv_sf_condicoes <- list(
  cartao_valid = cond_eq('cartao_ressarc', 'Sim'),
  psychlops_suic_1_1_sessao_final = cond_eq('psychlops_suic_1_sessao_final', 'Sim'),
  psychlops_suic_2_sessao_final = cond_eq('psychlops_suic_1_sessao_final', 'Sim'),
  psychlops_suic_3_sessao_final = cond_eq('psychlops_suic_1_sessao_final', 'Sim'),
  psychlops_suic_3_1_sessao_final = cond_in('psychlops_suic_3_sessao_final',
                                            c('Sim', 'Não tenho certeza')),
  enc_sessao_superv_obs = cond_eq('enc_sessao_superv', 'Sim'),
  enc_sessao_superv_apto = cond_eq('enc_sessao_superv', 'Sim'),
  enc_sessao_atend_ind_ql___1 = cond_eq('enc_sessao_atend_ind', 'Sim'),
  enc_sessao_atend_ind_ql___2 = cond_eq('enc_sessao_atend_ind', 'Sim'),
  enc_sessao_atend_ind_ql___3 = cond_eq('enc_sessao_atend_ind', 'Sim'),
  enc_sessao_atend_ind_ql___4 = cond_eq('enc_sessao_atend_ind', 'Sim')
)

ausentes_interv_sf_escopo <- c(
  setNames(rep(list(interv_sf_realiz_ids), 
               length(ausentes_interv_sf_vars)), 
           ausentes_interv_sf_vars)
)


# checagem de sanidade
stopifnot(!any(duplicated(names(ausentes_interv_sf_escopo))))

ausentes_interv_sf_df <- checar_faltantes(
  df |>
    filter(
      redcap_event_name == "Sessao final (Arm 1: Participantes)" &
        record_id %in% interv_sf_realiz_ids), 
  id_col = "record_id", 
  vars = ausentes_interv_sf_vars, 
  condicoes = ausentes_interv_sf_condicoes)