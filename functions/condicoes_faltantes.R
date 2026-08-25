cond_eq <- function(col, valor) {
  force(col); force(valor)   # evita o bug de lazy evaluation em loops
  function(row) {
    val <- row[[col]]
    if (is.null(val) || length(val) != 1) return(FALSE)
    isTRUE(val %in% valor)
  }
}

cond_in <- function(col, valores) {
  force(col); force(valores)
  function(row) {
    val <- row[[col]]
    if (is.null(val) || length(val) != 1) return(FALSE)
    isTRUE(val %in% valores)
  }
}

cond_na <- function(col) {
  function(row) !is.na(row[[col]])
}

cond_or <- function(...) {
  conds <- list(...)
  function(row) {
    any(purrr::map_lgl(conds, ~ {
      if (is.function(.x)) .x(row) else !is.na(row[[.x]])
    }))
  }
}
