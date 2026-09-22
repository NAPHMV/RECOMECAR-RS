#' Cria lógica de Download de dados para dashboard em shiny 
#'
#' @param data `dataframe`. Dados a serem salvos.
#' @param file_name `character`. Nome do arquivo final.
#' @param login `logical`. Indica se um login é necessário para exibir os dados.
#'
#' @return
#' @export
#'
#' @examples
render_download <- function(data, filename_prefix, login = reactive(TRUE)) {
  download <- downloadHandler(
    filename = function() {
      # Get filename if logged in
      req(login()) 
      paste0(
        filename_prefix,
        stringr::str_replace_all(Sys.Date(), ":", "-"),
        ".xlsx"
      ) 
    },
    content = function(file) {
      # Save data if logged in
      req(login())
      writexl::write_xlsx(data, path = file)
    }
  )
  
  return(download)
}