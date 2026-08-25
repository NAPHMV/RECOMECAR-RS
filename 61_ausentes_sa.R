# Variáveis para checar ======================================================
# unique(data_dictionary$form_name)

ausentes_interv_sa_vars <- data_dictionary |>
  filter(field_name %in% c(
    # Consentimento ----------------------------------------------------
    "confirm_gravar", "consent_adol", "consent_resp", "consent_pesquisas", "tcle_aceite", 
    # Termo Ressarcimento ----------------------------------------------
    'termo_cartao_consentimento', 'termo_cartao_nome', 'termo_cartao_envio___1',
    'termo_cartao_envio___2', #'termo_cartao_dta',
    # Envio do cartão e Kit --------------------------------------------
    #'ressarc_comp', 'ressarc_acordo',
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
    'psychlops_q1_2', 'psychlops_q2_2', 'psychlops_q3_2', 'psychlops_q4_1',
    'psychlops_score_1', 'psychlops_score_2',
    # APS --------------------------------------------------------------
    'aps_q1', 'aps_q1_1', 'aps_q2', 'aps_q2_2', 'aps_q3', 'aps_q3_3',
    # ITMNS ------------------------------------------------------------
    'itmns_q1', 'itmns_q2', 'itmns_q3', 'imns_interv_protoc', 'imns_interv_protoc_detalhar',
    # Encerramento -----------------------------------------------------
    'enc_sa_agend_data', 'enc_sa_data', 'enc_sa_motivo', 'enc_sa_motivo_otr',
    'enc_sa_superv', 'enc_sa_superv_obs', 'enc_sa_superv_apto', 'enc_sa_atend_ind',
    'enc_sa_atend_ind_ql___1', 'enc_sa_atend_ind_ql___2', 'enc_sa_atend_ind_ql___3',
    'enc_sa_atend_ind_ql___4'
  )) |>
  pull(field_name)

ausentes_interv_sa_condicoes <- list(
  consent_adol = list(cond_eq('confirm_gravar', 'Sim'), cond_eq("maior_igual_18", "Sim")),
  consent_resp = list(cond_eq('confirm_gravar', 'Sim'), cond_eq("maior_igual_18", "Não")),
  consent_pesquisas = cond_eq('confirm_gravar', 'Sim'),
  tcle_aceite = cond_eq('confirm_gravar', 'Sim'),
  termo_cartao_nome = cond_eq('termo_cartao_consentimento', 'Sim, entendi e concordo'),
  termo_cartao_envio___1 = cond_eq('termo_cartao_consentimento', 'Sim, entendi e concordo'),
  termo_cartao_envio___2 = cond_eq('termo_cartao_consentimento', 'Sim, entendi e concordo'),
  #ressarc_comp = cond_eq('confirm_gravar', 'Sim'), 
  escalas_auto_gravacao_confirma = cond_or(
    cond_eq('consent_adol', "Sim, autorizo a gravação da minha imagem e/ou voz"), 
    cond_eq('consent_resp', "Sim, autorizo a gravação da imagem e/ou voz do adolescente pelo qual sou responsável")),
  aps_q1_1 = cond_eq('aps_q1', 'Sim'),
  aps_q2 = cond_eq('aps_q1', 'Sim'),
  aps_q2_2 = cond_eq('aps_q2', 'Sim'),
  aps_q3   = cond_eq('aps_q1', 'Sim'),
  aps_q3_3 = cond_or(cond_eq('aps_q3', 'Sim'), cond_eq('aps_q3', 'Não tenho certeza')),
  imns_interv_protoc_detalhar = cond_eq('imns_interv_protoc', 'Sim'),
  enc_sa_data = cond_eq('enc_sa_agend_data', 'Sim'),
  enc_sa_motivo = cond_eq('enc_sa_agend_data', 'Não'),
  enc_sa_motivo_otr = cond_eq('enc_sa_motivo', 'Outro'),
  enc_sa_superv_obs = cond_eq('enc_sa_superv', 'Sim'),
  enc_sa_superv_apto = cond_eq('enc_sa_superv', 'Sim'),
  enc_sa_atend_ind_ql___1 = cond_eq('enc_sa_atend_ind', 'Sim'),
  enc_sa_atend_ind_ql___2 = cond_eq('enc_sa_atend_ind', 'Sim'),
  enc_sa_atend_ind_ql___3 = cond_eq('enc_sa_atend_ind', 'Sim'),
  enc_sa_atend_ind_ql___4 = cond_eq('enc_sa_atend_ind', 'Sim')
)

ausentes_interv_sa_escopo <- c(
  setNames(rep(list(interv_sa_realiz_ids), 
               length(ausentes_interv_sa_vars)), 
           ausentes_interv_sa_vars)
)


# checagem de sanidade
stopifnot(!any(duplicated(names(ausentes_interv_sa_escopo))))

ausentes_interv_sa_df <- checar_faltantes(
  df |>
    filter(
      redcap_event_name == "Sessao de apresentação (Arm 1: Participantes)" &
        record_id %in% interv_sa_realiz_ids), 
  id_col = "record_id", 
  vars = ausentes_interv_sa_vars, 
  condicoes = ausentes_interv_sa_condicoes)