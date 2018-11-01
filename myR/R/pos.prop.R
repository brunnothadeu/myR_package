#' @title Calcula o posicionamento empirico
#' @name pos.prop
#' 
#' @description Calcula o posicionamento empirico dos itens atraves das respostas observadas.
#'
#' @param dadosCorr Banco de dados corrigido a ser utilizado. Este deve ser composto com os seguintes campos: (IT1, ..., ITm).
#' @param desemp Medida de desempenho a ser utilizada.
#' @param PESO Vetor com os pesos referentes aos casos de 'dadosCorr'.
#' @param breaks Limite inferior e superior dos pontos de posicionamento, acompanhados pelo step da sequencia.
#' @param crit Lista com os criterios para posicionamento.
#' \itemize{
#'  \item{PROP}{: Proporcao minima de respostas corretas no nivel}.
#'  \item{NRESP}{: Numero minimo de respostas no nivel}.
#'  \item{PROP2}{: Proporcao maxima de respostas corretas no nivel anterior}.
#'  \item{DIF}{: Diferenca minima entre PROP e PROP2}.
#' }
#' 
#' @details Etapa Anterior: 'escore'.
#' 
#' @return Vetor com o posicionamento (se possivel) de cada item.
#' 
#' @author Brunno Bittencourt
#'
#' @seealso pos.model()
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
#' dadosCorr = escore(dadosCorr)
#'
#' POS = pos.prop(dadosCorr[paste0("IT", 1:6)], 100 / 6 * dadosCorr$Escore, breaks = c(0, 100, 10), crit = list(PROP = 0.65, NRESP = 20))
#' 
#' @import magrittr
#' @importFrom SDMTools wt.mean
#' @importFrom dplyr anti_join
#' @export
pos.prop <-
  function(dadosCorr, desemp, PESO = NULL, breaks = c(-5, 5, 0.1), crit = list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0)){
    comp = data.frame(Name = c("PROP", "NRESP", "PROP2", "DIF"), Value = as.vector(unlist(list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0))), stringsAsFactors = F) %>%
      anti_join(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F), by = "Name") %>%
      rbind(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F))
    crit = comp$Value %>% split(comp$Name)  
    if(is.null(PESO)) PESO = rep(1, length(desemp))
    desemp = cut(desemp, breaks = c(seq(breaks[1], breaks[2], breaks[3]) - breaks[3] / 2, breaks[2] + breaks[3] / 2), 
                 labels = seq(breaks[1], breaks[2], breaks[3]))
    POS = data.frame(matrix(NA, nrow = ncol(dadosCorr), ncol = length(levels(desemp)))); names(POS) = levels(desemp)
    for(it in seq_along(dadosCorr))
      POS[it, ] = data.frame(IT = dadosCorr[[it]], PESO = PESO) %>% split(desemp) %>% lapply(FUN = function(x) wt.mean(x[, 1], x[, 2])) %>% unlist
    pos = rep(NA, nrow(POS))
    POS[is.na(POS)] = 0
    for(it in 1:nrow(POS)){
      for(p in names(POS)){
        if(is.na(POS[it, p]))
          next
        if(sum(!is.na(dadosCorr[[it]][desemp == p])) >= crit$NRESP & POS[it, p] >= crit$PROP & 
           ifelse(which(names(POS) == p) > 1, POS[it, (which(names(POS) == p) - 1)] < crit$PROP2, FALSE) &
           ifelse(which(names(POS) == p) > 1, (POS[it, p] - POS[it, (which(names(POS) == p) - 1)]) >= crit$DIF, TRUE)){
          pos[it] = p
          break
        }
      }
    }
    return(pos)
  }
