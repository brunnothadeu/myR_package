#' @title Checa os itens com DIF e mal ajuste
#' @name checkDIF
#' 
#' @description Dado o resultado da calcDIF, esta funcao checa se ha, e se houver, quais itens possuem um DIF entre grupos ou com o modelo maior que o criterio especificado.
#'
#' @param EXP Objeto resultante da funcao calcDIF.
#' @param SCO DF contendo os seguintes campos: (Grupo, SCO) com as proficiencias dos individuos presentes na analise.
#' @param newGroup Vetor com os grupos de interesse (so serao plotados os itens com pelo menos um grupo de interesse).
#' @param crit Diferenca minima para caracterizar um comportamento diferencial.
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
checkDIF <-
  function(EXP, SCO, newGroup, crit = .15, probs = c(.05, .95)){
    SCO = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(probs[1], probs[2]))
    DIF = matrix(NA, ncol = 5)
    Ajuste = matrix(NA, ncol = 4)
    for(i in seq_along(EXP)){
      temp = EXP[[i]]
      for(g in unique(temp$Grupo)){
        for(gNew in newGroup){
          AUX = temp[temp$Grupo == g & temp$POINT >= max(SCO[[g]][1], SCO[[gNew]][1]) & temp$POINT <= min(SCO[[g]][2], SCO[[gNew]][2]), ]
          if(max(c(AUX[[paste0("DIF", gNew)]], 0), na.rm = T) >= crit)
            DIF = rbind(DIF, c(AUX$Item[1], c(g, gNew)[order(c(g, gNew))], AUX$POINT[AUX[[paste0("DIF", gNew)]] == max(AUX[[paste0("DIF", gNew)]], na.rm = T)], max(AUX[[paste0("DIF", gNew)]])))
        }
        AUX = temp[temp$Grupo == g & temp$POINT >= SCO[[g]][1] & temp$POINT <= SCO[[g]][2], ]
        if(max(c(AUX[["Ajuste"]], 0), na.rm = T) >= crit)
          Ajuste = rbind(Ajuste, c(AUX$Item[1], g, AUX$POINT[which(AUX[["Ajuste"]] == max(AUX[["Ajuste"]]))], max(AUX[["Ajuste"]])))
      }
    }
    DIF = data.frame(matrix(DIF[-1, ], ncol = 5, byrow = FALSE), stringsAsFactors = F); names(DIF) = c("Item", "Grupo1", "Grupo2", "Ponto", "DIF")
    DIF$Ponto <- as.numeric(DIF$Ponto); DIF$DIF <- as.numeric(DIF$DIF)
    Ajuste = data.frame(Ajuste[-1, ], stringsAsFactors = F); names(Ajuste) = c("Item", "Grupo", "Ponto", "DIF")
    Ajuste$Ponto <- as.numeric(Ajuste$Ponto); Ajuste$DIF <- as.numeric(Ajuste$DIF)
    return(list(DIF = unique(DIF), Ajuste = unique(Ajuste)))
  }