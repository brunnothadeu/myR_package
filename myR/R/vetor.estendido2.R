#' @title Cria um banco de dados em formato estendido
#' @name vetor.estendido2
#' 
#' @description Utilizando o codigo dos itens que compoem os cadernos, esta funcao tem como proposito criar um banco de dados no formato estendido.
#' 
#' @param dados Banco de dados a ser aberto. Este deve ser composto com os seguintes campos: (ID, CAD, PESO, IT1, ..., ITn).
#' @param configCADS Lista com os vetores de codigos para cada caderno. Os nomes de cada vetor dentro da lista deve ser igual ao respectivo valor no campo 'CAD' dentro dos dados.
#' 
#' @details Etapa Posterior: 'corrigir'.
#' 
#' @return DF com a seguinte composicao: (ID, CAD, PESO, IT1, ..., ITn, ..., ITm).
#' 
#' @author Brunno Bittencourt
#' 
#' @examples 
#' configCADS = list(CAD1 = c(1, 2, 3, 4), CAD2 = c(3, 4, 5, 6), CAD3 = c(5, 6, 1, 2))
#' 
#' resp = data.frame(matrix(sample(c("A", "B", "C"), 40, replace = T), ncol = 4), stringsAsFactors = F); names(resp) = paste0("IT", 1:4)
#'
#' dados = cbind(data.frame(ID = 1:10, CAD = paste0("CAD", sample(3, 10, replace = T)), PESO = 1), resp)
#'
#' dados = vetor.estendido2(dados, configCADS)
#' 
#' @import magrittr
#' @export
vetor.estendido2 <-
  function(dados, configCADS){
    CodIts = configCADS %>% unlist %>% unique
    CodIts = CodIts[order(CodIts, decreasing = F)]
    aux = data.frame(matrix(NA, nrow = nrow(dados), ncol = length(CodIts)))
    names(aux) = paste0("IT_", CodIts)
    for(cad in names(configCADS))
      aux[dados$CAD == cad, paste0("IT_", configCADS[[cad]])] = dados[dados$CAD == cad, 4:ncol(dados)]
    aux = cbind(dados[1:3], aux)
    return(aux)
  }
