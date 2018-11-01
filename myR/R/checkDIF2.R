#' @title Checa os itens com DIF e mal ajuste
#' @name checkDIF2
#' 
#' @description Funcao em desenvolvimento. Implementacao do argumento minresp.
#'
#' @param EXP Objeto resultante da funcao calcDIF.
#' @param SCO DF com o Grupo e o SCO de cada individuo.
#' @param newGroup Vetor com os grupos de interesse (so serao plotados os itens com pelo menos um grupo de interesse).
#' @param crit Diferenca minima para caracterizar um comportamento diferencial.
#' @param minresp Numero minimo de respostas para utilizacao do ponto de quadratura em questao.
#' @param probs Quantiles utilizados para o calculo dos intervalos de interesse.
#'
#' @details Apenas serao consideradas as proporcoes entre os quantis passados no argumento SCO.
#' @details Etapa Anterior: 'calcDIF' 'read.SCO'.
#' @details Etapa Posterior: 'dif.plot'.
#'
#' @return Lista com dois DFs, apresentando os itens que apresentaram comportamento diferencial e mal ajuste.
#'
#' @author Brunno Bittencourt
#' 
#' @examples 
#' EXP = remakeEXP(readLines("EXPtest.EXP"))
#'
#' newGroup = c(6, 7)
#'
#' EXP = calcDIF(EXP, newGroup)
#'
#' SCO = read.SCO(readLines("SCOtest.SCO"))
#'
#' SCO = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(.05, .95))
#'
#' DIF = checkDIF(EXP, SCO, newGroup)
#'
#' @export
checkDIF2 <-
  function(EXP, SCO, newGroup, crit = .15, minresp = 20, probs = c(.05, .95)){
    quants = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(probs[1], probs[2]))
    DIF = matrix(NA, ncol = 5)
    for(i in seq_along(EXP)){
      temp = EXP[[i]]
      for(gNew in newGroup){
        if(gNew %in% temp$Grupo){
          x = unique(temp[temp$Grupo == gNew, "POINT"])
          dif = (x[-1] - x[-length(x)]) / 2
          max = x + c(dif, mean(dif))
          zNew = data.frame(table(cut(SCO$SCO[SCO$Grupo == gNew], breaks = c(x[1] - mean(dif), max), labels = x)))
          zNew$Var1 = x
          for(g in unique(temp$Grupo)){
            AUX = temp[temp$Grupo == g & temp$POINT >= max(quants[[g]][1], quants[[gNew]][1]) & temp$POINT <= min(quants[[g]][2], quants[[gNew]][2]), ]
            x = unique(temp[temp$Grupo == g, "POINT"])
            dif = (x[-1] - x[-length(x)]) / 2
            max = x + c(dif, mean(dif))
            z = data.frame(table(cut(SCO$SCO[SCO$Grupo == g], breaks = c(x[1] - mean(dif), max), labels = x)))
            z$Var1 = x
            AUX = AUX[AUX$POINT %in% c(zNew$Var1[zNew$Freq > minresp], z$Var1[z$Freq > minresp]), ]
            if(max(c(AUX[[paste0("DIF", gNew)]], 0), na.rm = T) >= crit)
              DIF = rbind(DIF, c(AUX$Item[1], c(g, gNew)[order(c(g, gNew))], AUX$POINT[AUX[[paste0("DIF", gNew)]] == max(AUX[[paste0("DIF", gNew)]], na.rm = T)], max(AUX[[paste0("DIF", gNew)]])))
          }
        }
      }
    }
    DIF = data.frame(DIF[-1, ]); names(DIF) = c("Item", "Grupo1", "Grupo2", "Ponto", "DIF")
    return(unique(DIF))
  }