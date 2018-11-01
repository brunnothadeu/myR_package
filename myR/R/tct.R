#' @title Calcula medidas provindas da Teoria Classica dos Testes (TCT)
#' @name tct
#' 
#' @description Utilizando uma estrutura de BI ou BIB, calcula as estatisticas classicas dos itens.
#'
#' @param dados Banco de dados a ser utilizado. Este deve ser composto com os seguintes campos: (ID, CAD, PESO, IT1, ..., ITm, Desempenho).
#' @param dadosCorr Banco de dados corrigido a ser utilizado. Este deve ser composto com os seguintes campos: (ID, CAD, PESO, IT1, ..., ITm, Desempenho).
#' @param mapa DF com informacoes adicionais dos itens. Este deve estar ordenado na mesma ordem que 'dados' e 'dadosCorr', e obrigatoriamente deve conter uma coluna chama 'Gabarito'.
#' @param Alts Vetor com as possiveis marcacoes presentes nos dados.
#' @param arred Numero de casas decimais a serem consideradas no arredondamento.
#' @param sdpop Considerar o desvio populacional para o calculo das estatisticas?
#' @param summary Calcula um conjunto de informacoes sobre os dados utilizados nas estatisticas.
#'
#' @details Etapa Anterior: 'escore'.
#' @details Etapa Posterior: 'pos.prop'.
#' 
#' @return DF com a seguinte composicao: (mapa, Dif, Disc, Abai, Acim, Bis, PercAlts, BiseAlts). Caso summary = TRUE, retorna uma lista com os dois objetos.
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
#' dadosCorr = escore(dadosCorr)
#' 
#' dados$Escore = dadosCorr$Escore
#'
#' mapa = data.frame(Codigo = 1001:1006, Posicao = 1:6, Gabarito = c("A", "B", "A", "C", "A", "B"))
#' 
#' TCT = tct(dados, dadosCorr, mapa, b.len, nblocos, Alts = c("A", "B", "C"))
#'
#' @import magrittr
#' @importFrom SDMTools wt.mean
#' @importFrom SDMTools wt.sd
#' @export
tct <-
  function(dados, dadosCorr, mapa, Alts = c("A", "B", "C", "D", "E", "*", "."), arred = 10, sdpop = FALSE, summary = FALSE){
    TCT = data.frame(matrix(NA, nrow = ncol(dados) - 4, ncol = ncol(mapa) + 5 + length(Alts) * 2))
    names(TCT) = c(names(mapa), "Dif", "Disc", "Abai", "Acim", "Bis", paste0(rep(c("Perc", "Bise"), each = length(Alts)), Alts))
    TCT[1:ncol(mapa)] = mapa
    names(dadosCorr)[ncol(dadosCorr)] = names(dados)[ncol(dados)] = "Desempenho"
    if(summary){
      summ = data.frame(Info = c("Número total de respondentes", "Número de respondentes por item", "Desvio-Padrão por item", "Desempenho médio do grupo gabarito",
                                 "Desempenho médio do grupo não-gabarito","Quantile .27 do gabarito","Quantile .73 do gabarito"))
      summ = cbind(summ, matrix(NA, nrow = nrow(summ), ncol = nrow(mapa)))
      names(summ) = c("Info", paste0("IT", 1:nrow(mapa)))
    }
    for(it in 1:nrow(TCT)){
      respOn = dadosCorr[[it + 3]] %>% is.na %>% not
      if(sdpop){
        sd = sqrt(sum(dados$PESO[respOn]*((dados$Desempenho[respOn] - weighted.mean(dados$Desempenho[respOn], dados$PESO[respOn]))^2)) / sum(dados$PESO[respOn]))
      }else{
        sd = wt.sd(dados$Desempenho[respOn], dados$PESO[respOn])
      }
      if(summary){
        summ[1, it + 1] = nrow(dadosCorr)
        summ[2, it + 1] = sum(respOn)
        summ[3, it + 1] = sd
      }
      for(alt in Alts){
        respAlt = respOn & dados[[it + 3]] == alt
        TCT[it, paste0("Perc", alt)] = dados[respOn, it + 3] %>% equals(alt) %>% wt.mean(dadosCorr$PESO[respOn]) %>% round(arred)
        z = TCT[it, paste0("Perc", alt)] %>% qnorm %>% dnorm
        u1 = dados$Desempenho[respAlt] %>% wt.mean(dadosCorr$PESO[respAlt])
        u0 = dados$Desempenho[!respAlt & respOn] %>% wt.mean(dadosCorr$PESO[!respAlt & respOn])
        TCT[it, paste0("Bise", alt)] = TCT[it, paste0("Perc", alt)] %>% multiply_by(1 - TCT[it, paste0("Perc", alt)]) %>%
          multiply_by(u1 - u0) %>% divide_by(sd * z) %>% round(arred)
        if(alt == TCT$Gabarito[it]){
          quantile = dadosCorr$Desempenho[respOn] %>% quantile(probs = c(.27, .73))
          grupo = ifelse(dadosCorr$Desempenho <= quantile[1], 1, ifelse(dadosCorr$Desempenho >= quantile[2], 3, 2))
          TCT$Abai[it] = dadosCorr[respOn & grupo == 1, it + 3] %>% wt.mean(dadosCorr$PESO[respOn & grupo == 1]) %>% round(arred)
          TCT$Acim[it] = dadosCorr[respOn & grupo == 3, it + 3] %>% wt.mean(dadosCorr$PESO[respOn & grupo == 3]) %>% round(arred)
          TCT$Dif[it] = TCT[it, paste0("Perc", alt)]
          TCT$Bis[it] = TCT[it, paste0("Bise", alt)] 
          if(summary){
            summ[4, it + 1] = u1
            summ[5, it + 1] = u0  
            summ[6, it + 1] = quantile[1]
            summ[7, it + 1] = quantile[2]
          }
        }
      }
    }
    TCT$Disc = TCT$Acim %>% subtract(TCT$Abai) %>% round(arred)
    if(summary)
      TCT = list(tct = TCT, summary = summ)
    return(TCT)
  }