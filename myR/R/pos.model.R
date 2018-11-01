#' @title Calcula o posicionamento via modelo
#' @name pos.model
#' 
#' @description Calcula o posicionamento dos itens atraves do modelo 3PL.
#'
#' @param pars DF com os parametros dos itens. Deve ser composto com os seguintes campos: (a, b, c).
#' @param breaks Limite inferior e superior dos pontos de posicionamento, acompanhados pelo step da sequencia.
#' @param crit Lista com os criterios para posicionamento.
#' \itemize{
#'  \item{PROP}{: Proporcao minima de respostas corretas no nivel}.
#'  \item{PROP2}{: Proporcao maxima de respostas corretas no nivel anterior}.
#'  \item{DIF}{: Diferenca minima entre PROP e PROP2}.
#' }
#'
#' @return Vetor com o posicionamento (se possivel) de cada item.
#'
#' @author Brunno Bittencourt
#'
#' @seealso pos.prop()
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
#' pars = data.frame(Codigo = 1001:1006, a = abs(rnorm(6, mean = 1.5, sd = 0.5)), b = rnorm(6, mean = 0.5), c = runif(6, 0.1, 0.3))
#'				  
#' POS = pos.model(pars, crit = list(PROP = 0.6, DIF = 0.05))
#' 
#' @import magrittr
#' @importFrom dplyr anti_join
#' @export
pos.model <-
  function(pars, breaks = c(-5, 5, 0.1), crit = list(PROP = 0.65, PROP2 = 1, DIF = 0)){
    comp = data.frame(Name = c("PROP", "NRESP", "PROP2", "DIF"), Value = as.vector(unlist(list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0))), stringsAsFactors = F) %>%
      anti_join(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F), by = "Name") %>%
      rbind(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F))
    crit = comp$Value %>% split(comp$Name)  
    points = seq(breaks[1], breaks[2], breaks[3])
    POS = data.frame(matrix(NA, nrow = nrow(pars), ncol = length(points))); names(POS) = as.character(round(points, 1))
    for(it in 1:nrow(pars))
      POS[it, ] = pars$c[it] + (1 - pars$c[it]) / (1 + exp(-1.7 * pars$a[it] * (points - pars$b[it])))
    pos = rep(NA, nrow(POS))
    for(it in 1:nrow(POS)){
      for(p in names(POS)){
        if(POS[it, p] >= crit$PROP & 
           ifelse(which(names(POS) == p) > 1, POS[it, as.character(as.numeric(p) - breaks[3])] < crit$PROP2, FALSE) &
           ifelse(which(names(POS) == p) > 1, (POS[it, p] - POS[it, as.character(as.numeric(p) - breaks[3])]) >= crit$DIF, TRUE)){
          pos[it] = p
          break
        }
      }
    }
    return(pos)
  }