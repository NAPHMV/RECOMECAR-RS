#' Renderiza datatable para inclusão em dashboard shiny
#'
#' @param data `datatable`. Widget a ser renderizado pelo shiny.
#' @param login `logical`. Indica se um login é necessário para exibir os dados.
#'
#' @return Renderiza widget para tabela.
#' @export
#'
#' @examples
#' make_card(dt_itmns_interv, login = TRUE)
render_dt <- function(data, login = reactive(TRUE)) {
  # if (!"datatables" %in% attributes(data)$class) stop("Argumento ´data´ deve ter classe ´datatables´. Utilize a função auxiliar make_dt.R para gerar o widget com base em um dataframe.")
  # if (!is.logical(login)) stop("Argumento `login` deve ter classe `logical`, i.e. ser `TRUE` ou `FALSE`.")
  
  tabela <- renderDT({
    # Pass datatable if logged in
    req(login()); data
  })
  
  return(tabela)
}