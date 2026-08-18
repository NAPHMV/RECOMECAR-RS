# carrega_planilha <- function(url, aba, token) {
#   resp <- httr::GET(
#     url,
#     query = list(token = token, aba = aba),
#     httr::config(followlocation = TRUE, ssl_verifypeer = TRUE),
#     httr::timeout(100)
#   )
#   
#   if (httr::status_code(resp) != 200) {
#     stop("Erro HTTP ", httr::status_code(resp), ": ", httr::content(resp, "text", encoding = "UTF-8"))
#   }
#   
#   ct <- httr::headers(resp)$`content-type`
#   if (!grepl("application/json", ct, fixed = TRUE)) {
#     stop("Resposta não é JSON. Conteúdo recebido: ", 
#          substr(httr::content(resp, "text", encoding = "UTF-8"), 1, 300))
#   }
#   
#   jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
# }

carrega_planilha <- function(url, aba, token) {
  resp <- httr::GET(
    url,
    query = list(token = token, aba = aba),
    httr::add_headers(
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36"
    ),
    httr::config(followlocation = TRUE, ssl_verifypeer = TRUE),
    httr::timeout(100)
  )
  
  status <- httr::status_code(resp)
  ct <- httr::headers(resp)$`content-type`
  corpo <- httr::content(resp, "text", encoding = "UTF-8")
  
  if (status != 200) {
    stop("Erro HTTP ", status, " ao carregar aba '", aba, "'. ",
         "Content-Type: ", ct, ". Início do corpo: ",
         substr(corpo, 1, 300))
  }
  
  if (is.null(ct) || !grepl("application/json", ct, fixed = TRUE)) {
    stop("Resposta não é JSON para aba '", aba, "'. Content-Type: ", ct,
         ". Início do corpo: ", substr(corpo, 1, 300))
  }
  
  jsonlite::fromJSON(corpo)
}