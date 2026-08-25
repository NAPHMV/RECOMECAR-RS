checar_faltantes <- function(df, id_col, vars, condicoes = list(), escopo = list()) {
  
  avaliar_condicao <- function(cond, row) {
    if (is.function(cond)) {
      cond(row)
    } else if (is.character(cond)) {
      !is.na(row[[cond]])
    } else {
      stop("condição não reconhecida: ", class(cond))
    }
  }
  
  df %>%
    pmap_dfr(function(...) {
      row <- list(...)
      id_val <- row[[id_col]]
      
      faltantes <- purrr::keep(vars, function(v) {
        
        ids_permitidos <- escopo[[v]]
        no_escopo <- if (is.null(ids_permitidos)) TRUE else id_val %in% ids_permitidos
        
        dep <- condicoes[[v]]
        checar_dep <- if (is.null(dep)) {
          TRUE
        } else if (is.list(dep) && !is.function(dep)) {
          all(purrr::map_lgl(dep, avaliar_condicao, row = row))
        } else {
          isTRUE(avaliar_condicao(dep, row))
        }
        
        no_escopo && isTRUE(checar_dep) && is.na(row[[v]])
      })
      
      tibble(
        ID = id_val,
        `Dados faltantes` = paste(faltantes, collapse = ", ")
      )
    })
}