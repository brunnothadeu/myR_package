#' @title Cria um banco de dados em formato estendido
#' @name vetor.estendido
#' 
#' @description Utilizando uma estrutura de BI ou BIB, esta funcao tem como proposito criar um banco de dados no formato estendido.
#' 
#' @param dados Banco de dados a ser aberto. Este deve ser composto com os seguintes campos: (ID, CAD, PESO, IT1, ..., ITn).
#' @param bib Lista com a estrutura de cada caderno. Os nomes de cada vetor dentro da lista deve ser igual ao respectivo valor no campo 'CAD' dentro dos dados.
#' @param nblocos Numero de blocos no teste.
#' @param b.len Numero de itens em cada bloco.
#' 
#' @details Etapa Posterior: 'corrigir'.
#' 
#' @return DF com a seguinte composicao: (ID, CAD, PESO, IT1, ..., ITn, ..., ITm).
#' 
#' @author Brunno Bittencourt
#' 
#' @examples 
#' bib = list(CAD1 = (1,2), CAD2 = c(2,3), CAD3 = c(3,1))
#' 
#' nblocos = 3
#'
#' b.len = 2
#'
#' resp = data.frame(matrix(sample(c("A", "B", "C"), 40, replace = T), ncol = 4), stringsAsFactors = F); names(resp) = paste0("IT", 1:4)
#'
#' dados = cbind(data.frame(ID = 1:10, CAD = paste0("CAD", sample(3, 10, replace = T)), PESO = 1), resp)
#'
#' dados = vetor.estendido(dados, bib, nblocos, b.len)
#' 
#' @import magrittr
#' @export
vetor.estendido <-
  function(dados, bib, nblocos, b.len){
    if(sum(dados$CAD %in% names(bib)) == 0)
      stop("Os valores de Cadernos entre o banco e o mapa não conferem.")
    aux = data.frame(matrix(NA, nrow = nrow(dados), ncol = b.len * nblocos))
    for(cad in names(bib)){
      ind =  bib[[cad]] %>% subtract(1) %>% multiply_by(b.len - 1) %>% add(bib[[cad]])
      ind = unlist(lapply(ind, FUN = function(x, b.len){seq(x, x + b.len - 1)}, b.len = b.len))
      aux[dados$CAD == cad, ind] = dados[dados$CAD == cad, 4:ncol(dados)]
    }
    aux = cbind(dados[1:3], aux); names(aux) = c("ID", "CAD", "PESO", paste0("IT", 1:(nblocos * b.len)))
    return(aux)
  }

