# Baseline =====================================================================
ausentes_facilit_baseline_vars <- data_dictionary |>
  filter(field_name %in% c(
    ## Elegibilidade e Consentimento --------------------------------
    df |> select(inclu_facilit_1:exclu_facilit_3) |> names(),
    df |> select(assinou_tcle_facilitador:upload_tcle_facilitador) |> names(),
    "envio_survey_dados_pesso",
    # Dados de Base -------------------------------------------------
    df |> select(id_facilitador, email_facilitador) |> names(),
    # Dados sociodemográficos ---------------------------------------
    df |> select(confirm_id_facilitador:tel_cont_facilitador) |> names(),
    # Experiência nas Enchentes ---------------------------------------
    df |> select(trab_volunt_facilitador:isolado_facilitador) |> names()
  )) |>
  pull(field_name)

ausentes_facilit_baseline_condicoes <- list(
  assinou_tcle_facilitador = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  tcle_consentiu_questionarios = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  tcle_consentiu_imagem_voz = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  tcle_consentiu_roleplay = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  tcle_consentiu_pesquisa_futura = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  upload_tcle_facilitador = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  envio_survey_dados_pesso = cond_eq('calc_eleg_facilitador', 1) &
    cond_na('exclu_facilit_3'),
  otr_id_facilitador = cond_eq('confirm_id_facilitador', "Não"),
  otr_nome_facilitador = cond_eq('confirm_nome_facilitador', "Não"),
  otr_telefone_facilitador = cond_eq('confirm_tel_facilitador', "Não"),
  otr_email_facilitador = cond_eq('confirm_email_facilitador', "Não"),
  outro_gen_facilitador = cond_eq('genero_facilitador', "Outro {outro_gen_facilitador}"),
  outra_area_est_facilitador = cond_eq('area_est_facilitador', 'Outro {outra_area_est_facilitador}'),
  ql_trab_volunt_facilitador = cond_eq('trab_volunt_facilitador', 'Sim'),
  otr_trab_volunt_facilit = cond_eq('ql_trab_volunt_facilitador___6', 'Checked'),
  local_facilitador___0 = cond_eq('sair_casa_facilitador', 'Sim'),
  local_facilitador___1 = cond_eq('sair_casa_facilitador', 'Sim'),
  local_facilitador___2 = cond_eq('sair_casa_facilitador', 'Sim'),
  local_facilitador___3 = cond_eq('sair_casa_facilitador', 'Sim'),
  tempo_fora_facilitador = cond_eq('sair_casa_facilitador', 'Sim'),
  permanece_facilitador = cond_eq('sair_casa_facilitador', 'Sim'),
  onde_permanece_facilitador___0 = cond_eq('permanece_facilitador', 'Sim'),
  onde_permanece_facilitador___1 = cond_eq('permanece_facilitador', 'Sim'),
  onde_permanece_facilitador___2 = cond_eq('permanece_facilitador', 'Sim'),
  onde_permanece_facilitador___3 = cond_eq('permanece_facilitador', 'Sim'),
)

ausentes_facilit_baseline_escopo <- c(
  setNames(
    rep(
      list(interv_s4_realiz_ids), 
      length(
        ausentes_facilit_baseline_vars[!ausentes_facilit_baseline_vars %in% "escalas_auto_gravacao_confirma"])), 
    ausentes_facilit_baseline_vars[!ausentes_interv_s4_vars %in% "escalas_auto_gravacao_confirma"]
  )
)