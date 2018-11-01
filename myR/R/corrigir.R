#' @title Corrige os dados de um DF
#' @name corrigir
#' 
#' @description Corrige um banco de dados gerado pelas funcoes de extensao.
#' 
#' @param dados Banco de dados a ser corrigido. Este deve ser composto com os seguintes campos: (ID, CAD, PESO, IT1, ..., ITm).
#' @param gab Vetor com os gabaritos de cada item ordenado na mesma ordem de 'dados'.
#' 
#' @details Etapa Anterior: 'vetor.extendido' 'vetor.extendido2'.
#' @details Etapa Posterior: 'escore'.
#' 
#' @return DF com a seguinte composicao: (ID, CAD, PESO, IT1, ..., ITn, ..., ITm).
#' 
#' @author Brunno Bittencourt
#' 
#' @examples 
#' bib = list(CAD1 = c(1,2), CAD2 = c(2,3), CAD3 = c(3,1))
#' 
#' nblocos = 3
#'
#' b.len = 2
#'
#' resp = data.frame(matrix(sample(c("A", "B", "C"), 40, replace = T), ncol = 4), stringsAsFactors = F); names(resp) = paste0("IT", 1:4)
#'
#' dados = cbind(data.frame(ID = 1:10, CAD = paste0("CAD", sample(3, 10, replace = T)), PESO = 1), resp)
#'
#' dados = vetor.extendido(dados, bib, nblocos, b.len)
#' 
#' dadosCorr = corrigir(dados, c("A", "B", "A", "C", "A", "B"))
#' 
#' @export
corrigir <-
  function(dados, gab){
    dadosCorr = cbind(dados[1:3], data.frame(matrix(NA, nrow = nrow(dados), ncol = ncol(dados) - 3)))
    for(j in 4:ncol(dadosCorr))
      dadosCorr[j] = ifelse(dados[[j]] == gab[j - 3], 1, 0)
    names(dadosCorr) = names(dados)
    return(dadosCorr)
  }
