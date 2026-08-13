carrega_planilha <- function(url, aba, token) {
  resp <- httr::GET(
    url,
    query = list(token = token, aba = aba),
    config(followlocation = TRUE, ssl_verifypeer = TRUE),
    timeout(30)
  )
  
  if (status_code(resp) != 200) {
    stop("Erro HTTP ", status_code(resp), ": ", content(resp, "text", encoding = "UTF-8"))
  }
  
  ct <- httr::headers(resp)$`content-type`
  if (!grepl("application/json", ct, fixed = TRUE)) {
    stop("Resposta não é JSON. Conteúdo recebido: ", 
         substr(content(resp, "text", encoding = "UTF-8"), 1, 300))
  }
  
  jsonlite::fromJSON(content(resp, "text", encoding = "UTF-8"))
}