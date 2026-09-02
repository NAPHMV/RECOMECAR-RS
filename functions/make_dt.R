#' Cria datatable para dashboard a partir de um dataframe
#'
#' @param data `dataframe`. Dados a serem transformados.
#' @param preset `numeric`. Preset de datatable a ser usado.
#'
#' @return Retorna objeto datatable equivalente a `data` pronto para inclusão
#' no shiny
#' @export
#'
#' @examples
#' make_dt(dados_interv_itmns, preset = 1)
make_dt <- function(data, 
                    preset = 1, 
                    change_last_col_size = FALSE, 
                    ...) {
  # dataframe check
  if (!is.data.frame(data)) stop("Argumento `data` deve ter classe `dataframe` para gerar widget corretamente.")
  
  # coldefs
  coldefs <- list(
    list(className = 'dt-center', targets = '_all')
  )
  if (change_last_col_size) {
    coldefs[[2]] <- list(width = '200px', targets = -1)
  }
  
  # options
  options_extra <- list(...)
  
  options_default <- list(
    scrollX        = TRUE,
    fixedHeader    = TRUE,
    fixedColumns   = list(leftColumns = 1),
    position       = "left",
    dom            = 'ft',
    pageLength     = nrow(data),
    columnDefs     = coldefs,
    initComplete   = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '85%'});",
      "}"
    )
  )
  
  options_final <- modifyList(options_default, options_extra)
  
  # preset 1
  if (preset == 1) {
    dt <- DT::datatable(
      data,
      rownames   = NULL, 
      # filter     = TRUE,
      class      = 'cell-border stripe',
      extensions = c('FixedHeader', 'FixedColumns'),
      options    = options_final
    ) %>%
      DT::formatStyle(columns = c(1:ncol(data)), fontSize = '85%')   
  }
  
  return(dt)
}