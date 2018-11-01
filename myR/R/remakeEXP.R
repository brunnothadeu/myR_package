#' @title Le um arquivo .EXP
#' @name remakeEXP
#' 
#' @description Le e manipula um arquivo .EXP para a forma requerida na etapa posterior.
#'
#' @param EXP Objeto resultante da funcao readLines {base}.
#'
#' @details Etapa Posterior: 'calcDIF'.
#'
#' @return Lista com um DF reestruturado para cada item do arquivo.
#'
#' @author Brunno Bittencourt
#' 
#' @examples 
#' EXP = remakeEXP(readLines("EXPtest.EXP"))
#'
#' @import magrittr
#' @export
remakeEXP <-
  function(EXP){
    EXP = EXP[-c(1,2)] %>% subset(., . != paste(rep("-", 82), collapse = "")) %>% subset(., . != paste(rep("=", 82), collapse = "")) %>% gsub("\\s+", " ", .) %>% gsub("MODEL PROP", "MODEL.PROP", .)
    EXP %<>% split(unlist(lapply(strsplit(EXP, " "), FUN = function(x) x[1])))
    for(i in seq_along(EXP)){
      infos = unlist(strsplit(EXP[[i]], " "))
      AUX = matrix(NA, nrow = 5 * length(infos) / 56, ncol = 9)
      ini = 1
      for(j in seq(1, 5 * length(infos) / 56 - 4, 5)){
        AUX[j:(j+4), ] = cbind(rep(infos[ini], 5), rep(infos[ini + 1], 5), infos[ini+3:7], infos[ini+11:15], infos[ini+19:23], infos[ini+27:31], infos[ini+35:39], infos[ini+43:47], infos[ini+51:55])
        ini = ini + 56
      }
      AUX = data.frame(AUX, stringsAsFactors = F); names(AUX) = c("Item", "Grupo", "POINT", "WEIGHT", "TRIED", "RIGHT", "PROPORTION", "SE", "MODEL.PROP")
      AUX = AUX[order(AUX$Grupo, decreasing = F), ]
      options(warn = -1)
      for(j in 2:9)
        AUX[j] = as.numeric(AUX[[j]])
      options(warn = 0)
      EXP[[i]] = AUX
    }
    return(EXP)
  }
