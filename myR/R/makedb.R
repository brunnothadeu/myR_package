#' @title Formatar o banco de dados
#' @name makedb
#' 
#' @description Cria as colunas (ID, CAD, PESO) em um banco para montar o format requerido nas demais funcoes.
#' 
#' @param dados Banco de dados com as respostas.
#' 
#' @return DF com a seguinte composicao: (ID, CAD, PESO, IT1, ..., ITn, ..., ITm).
#' 
#' @author Brunno Bittencourt
#' 
#' @examples 
#' resp = data.frame(matrix(sample(c("A", "B", "C"), 40, replace = T), ncol = 4), stringsAsFactors = F); names(resp) = paste0("IT", 1:4)
#'
#' resp = makedb(resp)
#'
#' @export
makedb <-
  function(dados){
    return(data.frame(ID = 1:nrow(dados), CAD = 1, PESO = 1, dados))
  }