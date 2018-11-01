#' @title Le um arquivo .SCO
#' @name read.SCO
#' 
#' @description Le e manipula um arquivo .SCO para a forma requerida na etapa posterior.
#'
#' @param SCO Nome do arquivo a ser lido.
#' @param summary Printar 'table' dos grupos e 'summary' das proficiencias para fins de consistencia?
#' @param getID Manter IDs no resultado.
#' 
#' @details Etapa Posterior: 'cci.plot'.
#'
#' @return DF com o grupo e a proficiencia de cada individuo do arquivo.
#'
#' @author Brunno Bittencourt
#'
#' @examples 
#' SCO = read.SCO(readLines("SCOtest.SCO"))
#'
#' @import magrittr
#' @export
read.SCO <-
  function(SCO, summary = TRUE, getID = FALSE){
    SCO <- readLines(paste0(SCO, ".SCO"))
    SCO = SCO[-c(1,2)]
    n = SCO %>% length
    i = ifelse(substr(SCO[2], 7, 7) == "*", 6, 7)
    groups = SCO %>% subset(1:n %in% seq(1, n-1, 2)) %>% substr(3, 3)
    id = SCO %>% subset(1:n %in% seq(1, n - 1, 2))
    id = id %>% substr(6, nchar(id))
    SCO = SCO %>% subset(1:n %in% seq(2, n, 2)) %>% gsub("\\s+", " ", .) %>% strsplit(" ") %>% lapply(FUN = function(x) x[i]) %>% unlist %>% as.numeric
    if(summary){
      print(table(groups))
      print(summary(SCO))
    }
    SCO = data.frame(ID = id, Grupo = groups, SCO = SCO)
    if(!getID)
      SCO$ID = NULL
    return(SCO)
  }

