# Aba Gravações ================================================================
dados_gravacoes_raw <- carrega_planilha(url_gravacoes, "ControleGravacoes", token_gravacoes)
names(dados_gravacoes_raw) <- c("ID", "Código", "Sessão A", "Sessão A - Data",
                                "Sessão 1", "Sessão 1 - Data", "Sessão 2", "Sessão 2 - Data",
                                "Sessão 3", "Sessão 3 - Data", "Sessão 4", "Sessão 4 - Data",
                                "Sessão 5", "Sessão 5 - Data", "Sessão F", "Sessão F - Data")

dados_gravacoes_raw_long <- dados_gravacoes_raw |>
  rename_with(~ str_replace(.x, " - Data$", "__data"), starts_with("Sessão")) |>
  rename_with(~ str_replace(.x, "^(Sessão .+)$", "\\1__desfecho"), starts_with("Sessão") & !ends_with("__data")) |>
  select(-ends_with("data")) |>
  pivot_longer(
    cols = starts_with("Sessão"), 
    names_to = c("sessao", ".value"),
    names_sep = "__"
  ) |>
  mutate(
    sessao = fct_relevel(as.factor(sessao), "Sessão A", "Sessão 1", "Sessão 2", "Sessão 3",
                         "Sessão 4", "Sessão 5", "Sessão F")
  )

# Aba Facilitadores ===========================================================
dados_gravacoes_facilit <- carrega_planilha(url_gravacoes, "Facilitadores", token_gravacoes)
names(dados_gravacoes_facilit) <- c("codigo", "info", "facilit_nome", "superv_nome")