#' @title Calcula as diferencas entre as proporcoes
#' @name calcDIF
#' 
#' @description Para cada DF dentro da lista resultante da funcao 'remakeEXP', calcula as diferencas entre as PROPORTIONs dos grupos de interesse.
#'
#' @param EXP Objeto resultante da funcao remakeEXP.
#' @param newGroup Vetor com os grupos de interesse (so serao plotados os itens com pelo menos um grupo de interesse).
#'
#' @details Etapa Anterior: 'remakeEXP'.
#' @details Etapa Posterior: 'checkDIF'.
#'
#' @return Lista com um DF reestruturado para cada item do arquivo.
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
#' @import magrittr
#' @export
calcDIF <-
  function(EXP, newGroup){
    i = 1
    while(i <= length(EXP)){
      temp = EXP[[i]]
      groups = temp[["Grupo"]] %>% as.character %>% unique
      if(any(newGroup %in% groups)){
        temp[paste0("DIF", newGroup)] = NA
        for(gNew in newGroup[newGroup %in% groups])
          temp[paste0("DIF", gNew)] = temp[temp$Grupo == gNew, "PROPORTION"] %>% rep(length = nrow(temp)) %>% subtract(temp[["PROPORTION"]]) %>% abs
        temp$Ajuste = temp[["PROPORTION"]] %>% subtract(temp[["MODEL.PROP"]]) %>% abs
        EXP[[i]] = temp
        i = i + 1
      }else{
        EXP[[i]] = NULL
      }
    }
    return(EXP)
  }
