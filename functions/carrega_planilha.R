carrega_planilha <- function(url, aba, token) {
  resp <- httr::GET(
    url,
    query = list(token = token, aba = aba),
    httr::config(followlocation = TRUE, ssl_verifypeer = TRUE),
    httr::timeout(100)
  )
  
  if (httr::status_code(resp) != 200) {
    stop("Erro HTTP ", httr::status_code(resp), ": ", httr::content(resp, "text", encoding = "UTF-8"))
  }
  
  ct <- httr::headers(resp)$`content-type`
  if (!grepl("application/json", ct, fixed = TRUE)) {
    stop("Resposta não é JSON. Conteúdo recebido: ", 
         substr(httr::content(resp, "text", encoding = "UTF-8"), 1, 300))
  }
  
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
}