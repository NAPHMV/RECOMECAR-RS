#' Renderiza card para dashboard em shiny
#'
#' @param value Valor a ser exibido no card. 
#' @param subtitle `character`. Subtítulo utilizado na exibição do card.
#' @param login `logical`. Indica se um login é necessário para exibir os dados.
#' @param width `numeric`. Largura do card.
#' @param color `character`. Cor do card.
#'
#' @return
#' @export
#'
#' @examples
render_card <- function(value, subtitle, login = reactive(TRUE), width = 4, color = "purple") {
  card <- renderValueBox({
    # Render card if logged in
    req(login())
    valueBox(
      value = paste(value),
      subtitle = subtitle,
      width = 4,
      color = color
    )
  })
  
  return(card)
}