#' @title Abre strings de respostas
#' @name openstring
#' 
#' @description Gera uma data.frame com as respostas provindas de um vetor de strings.
#' 
#' @param dados Vetor contendo as strings de respostas.
#' 
#' @return DF com a seguinte composicao: (IT1, ..., ITn, ..., ITm).
#' 
#' @author Brunno Bittencourt
#' 
#' @examples 
#' resp = c("AABDAB", "DBDADD", "CCBDAD")
#'
#' resp = openstring(resp)
#'
#' @export
openstring <-
  function(dados){
    dados %<>% strsplit("") %>% unlist %>% matrix(nrow = length(dados), byrow = TRUE) %>% data.frame(stringsAsFactors = FALSE)
    names(dados) = paste0("IT", 1:ncol(dados))
    return(dados)
  }