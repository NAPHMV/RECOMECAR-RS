# Variáveis para checar ======================================================
# names(df)
data_dictionary <- exportMetaData(rcon)
# unique(data_dictionary$form_name)
ausentes_tri_forms <- c(
  "prtriagem", "triagem", "questes_sociodemogrficas",
  "hist_previa_sm", "gad7", "phq9", "pcl5", "ccas",
  "encerramento_triagem", "atendimento_psiquiatra",
  "atendimento_assistente_social", "monitoramento"
)

# data_dictionary |>
#   filter(form_name %in% ausentes_tri_forms) |>
#   View()

ausentes_tri_vars <- data_dictionary |>
  filter(form_name %in% ausentes_tri_forms) |>
  filter(field_name %in% c(
    # Contato pré-triagem
    "dta_preenchi_pre", "cod_pesq_1_busc", #"contato_realiz_1_busc", 
    # Pré-triagem
    "nome", "maior_igual_18", "num_fone", "email_menor",
    "nome_resp", "num_fone_resp", "email_resp",
    "telefone", "email", "possui_deficiencia",
    "sabe_cep", "cep", "nome_rua_2024", "bairro_2024", "cidade_2024",
    "sabe_cep_atual", "cep_atual", "nome_rua_atual", "bairro_atual",
    "cidade_atual", "sair_casa_enchente",
    # Consentimento para Triagem
    "calc_elegi_pre_triagem", # "particip_consentida", 
    "upload_tcle", "upload_tale",
    "upload_tcle_gravacao", "aceita_tcle", "consentimento_data",
    # Triagem Sociodemográfico
    "data_contato", "idade", "sexo_nasci", "genero_ident", "raca_cor",
    "nacionalidade", "escolaridade", "estado_civil", "possui_filhos",
    "qtd_pessoas_moram", "condicao_moradia", "coleta_lixo", "agua_canaliz",
    "rede_esgoto", "nome_familiar_confi", "trabalha_1_mes_antes",
    "trabalha_atualmente", "plano_saude", "dific_marcar_consulta",
    "dific_acesso_serv_saude", "alguem_contar_necess", "particip_atv_comunit",
    "vitima_descrim_violen", "beb_alcool", "usa_drogas",
    "perdeu_domicilio", "perdeu_moveis", "perdeu_docs", "perdeu_veiculo",
    "perdeu_trab_renda", "ferido_enchente", "perda_familiar",
    # História Prévia
    "encerram_med", "encerram_med_ql", "encerram_terapia", "encerram_prof",
    "encerram_prof_ql", "encerram_transt", "encerram_transt_ql"
  )) |>
  pull(field_name)

vars_socio <- c(
  "idade", "sexo_nasci", "genero_ident", "raca_cor", "nacionalidade",
  "escolaridade", "estado_civil", "possui_filhos", "qtd_pessoas_moram",
  "condicao_moradia", "coleta_lixo", "agua_canaliz", "rede_esgoto",
  "nome_familiar_confi", "trabalha_1_mes_antes", "trabalha_atualmente",
  "plano_saude", "dific_marcar_consulta", "dific_acesso_serv_saude",
  "alguem_contar_necess", "particip_atv_comunit", "vitima_descrim_violen",
  "beb_alcool", "usa_drogas", "perdeu_domicilio", "perdeu_moveis",
  "perdeu_docs", "perdeu_veiculo", "perdeu_trab_renda",
  "ferido_enchente", "perda_familiar"
)

vars_gad7 <- ausentes_tri_vars[grepl("^gad7", ausentes_tri_vars)]
vars_phq9 <- ausentes_tri_vars[grepl("^phq9", ausentes_tri_vars)]
vars_pcl5 <- ausentes_tri_vars[grepl("^pcl5", ausentes_tri_vars)]
vars_ccas <- ausentes_tri_vars[grepl("^ccas", ausentes_tri_vars)]

vars_encerramento <- ausentes_tri_vars[grepl("^encerram", ausentes_tri_vars)]

# Checa ====================================================================
# lista de dependências: nome_da_coluna = coluna_da_qual_depende
# ex: "telefone" só é considerado faltante se "possui_telefone" estiver preenchida
ausentes_tri_condicoes <- list(
  upload_tcle    = "abordagem",
  upload_tale    = 'abordagem',
  upload_tcle_gravacao = 'abordagem',
  aceita_tcle    = 'abordagem',
  maior_igual_16 = "maior_igual_18",
  num_fone       = "maior_igual_16",
  email_menor    = "maior_igual_16",
  nome_resp      = "maior_igual_16",
  num_fone_resp  = "maior_igual_16",
  email_resp     = "maior_igual_16",
  telefone       = "maior_igual_18",
  email          = "maior_igual_18",
  ql_deficiencia = "possui_deficiencia",
  cep            = cond_eq("sabe_cep", 'Sim'),
  nome_rua_2024  = cond_eq("sabe_cep", 'Não'),
  bairro_2024    = cond_eq("sabe_cep", 'Não'),
  cidade_2024    = cond_eq("sabe_cep", 'Não'),
  sabe_cep_atual = cond_eq('mora_mesmo_end', 'Não'),
  cep_atual      = cond_eq("sabe_cep_atual", 'Sim'),
  nome_rua_atual = cond_eq("sabe_cep_atual", 'Não'),
  bairro_atual   = cond_eq("sabe_cep_atual", 'Não'),
  cidade_atual   = cond_eq("sabe_cep_atual", 'Não'),
  encerram_med_ql = cond_eq("encerram_med", 'Sim (especificar): {encerram_med_ql}'),
  encerram_prof_ql = "encerram_prof",
  encerram_transt_ql = cond_eq("encerram_transt", 'Sim (especificar): {encerram_transt_ql}')
)
# junta tudo num único setNames por grupo
ausentes_tri_escopo <- c(
  setNames(rep(list(pretri_realiz_ids), length(vars_socio)), vars_socio),
  setNames(rep(list(tri_realiz_ids), length(vars_gad7)),  vars_gad7),
  setNames(rep(list(tri_realiz_ids), length(vars_phq9)),  vars_phq9),
  setNames(rep(list(tri_realiz_ids), length(vars_pcl5)),  vars_pcl5),
  setNames(rep(list(tri_realiz_ids), length(vars_ccas)),  vars_ccas),
  setNames(rep(list(tri_realiz_ids), length(vars_encerramento)), vars_encerramento)
)

# checagem de sanidade
stopifnot(!any(duplicated(names(ausentes_tri_escopo))))

ausentes_tri_df <- checar_faltantes(
  df |>
    filter(
      redcap_event_name == "Triagem (Arm 1: Participantes)" &
        record_id %in% tri_realiz_ids), 
  id_col = "record_id", 
  vars = ausentes_tri_vars, 
  condicoes = ausentes_tri_condicoes)