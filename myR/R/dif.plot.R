#' @title Gera os graficos de DIF
#' @name dif.plot
#' 
#' @description  Cria um diretorio chamado 'DIF' e gera os graficos de Funcionamento Diferenciado Do Item (DIF).
#'
#' @param EXP Objeto resultante da funcao calcDIF.
#' @param SCO Uma lista nomeada de acordo com os indices dos grupos em analise, contendo os percentis das distribuicoes de proficiencias.
#' @param main String ou vetor com o(s) titulo(s) dos graficos.
#' @param groups Vetor com o nome a ser plotado para cada um dos grupos, ordenado pelo indice do respectivo grupo.
#' @param probs Quantiles utilizados para o calculo dos intervalos de interesse.
#' @param col.dif Vetor com as cores a serem utilizadas nas curvas empiricas dos grupos.
#' @param density Plotar na imagem a densidade das proficiencias de cada grupo ('area', 'points'). Caso NULL, a densidade sera omitida.
#' @param dir.create Nome do diretorio a ser criado para guardar os graficos.
#' @param xlim Limites do eixo X a ser plotados.
#' @param height Altura em polegadas da imagem.
#' @param width Largura em polegadas da imagem.
#' @param shinyDemo Argumento auxiliar para o uso da funcao 'myR.app'.
#'
#' @details Etapa Anterior: 'checkDIF'.
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
#' dif.plot(EXP, SCO)
#'
#' @import magrittr
#' @import ggplot2
#' @importFrom grid pushViewport
#' @importFrom gridBase baseViewports
#' @importFrom randomcoloR distinctColorPalette
#' @export
dif.plot <-
  function(EXP, SCO, main = NULL, groups = NULL, probs = c(.05, .95), col.dif = NULL, density = "area", dir.create = "DIF", xlim = c(-4, 4), width = 9.6, height = 6.8, shinyDemo = NULL){
    if(is.null(groups))
      groups = paste0("Grupo", unique(SCO$Grupo))
    if(is.null(col.dif))
      col.dif = distinctColorPalette(length(groups))
    if(is.null(shinyDemo))
      dir.create(dir.create, showWarnings = FALSE)
    if(is.null(main))
      main = names(EXP)
    if(length(main) < length(EXP) & is.null(shinyDemo))
      stop("Dimensoes invalidas para main")
    if(!is.null(density)){
      habs = SCO
      if(density == "points"){
        habs = split(habs$SCO, habs$Grupo)
        for(i in seq_along(habs))
          habs[[i]] = density(habs[[i]])
      }
    }
    SCO = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(probs[1], probs[2]))
    for(i in seq_along(EXP)){
      if(is.null(shinyDemo)){
        pdf(file = paste0(getwd(), "/", dir.create,"/", paste0(names(EXP)[i], "_DIF.pdf")), width = width, height = height)
      }else{
        i = shinyDemo
      }
      if(!is.null(density)){
        layout(matrix(c(2, 1), ncol = 1, byrow = TRUE), widths = c(5/7, 2/7), heights = c(2/7, 5/7))
        if(density == "area")
          par(mar = c(4, 4, 1, 1), mai = c(1.02, 0.82, 0.02, 0.42))
        if(density == "points")
          par(mar = c(4.5, 4.5, 0, 1.5), bty = "o")
      }
      temp = EXP[[i]]
      nqp = nrow(temp) / (temp$Grupo %>% unique %>% length)
      plot(temp$POINT[1:nqp], seq(0, 1, length = nqp), type = "n", main = ifelse(is.null(density), main[i], ""), xlab = "Proficiencia", ylab = "Percentual de Respostas", ylim = c(0, 1), xlim = c(xlim[1], xlim[2]))
      lines(temp$POINT[1:nqp], temp$MODEL.PROP[1:nqp], lwd = 2)
      for(g in unique(temp$Grupo)){
        lines(temp$POINT[temp$Grupo == g], temp$PROPORTION[temp$Grupo == g], col = col.dif[g], lwd = 2)
        abline(v = SCO[[g]][1], lty = 2, col = col.dif[g])
        abline(v = SCO[[g]][2], lty = 2, col = col.dif[g])
      }
      legend(-4.2, 1, lty = 1, lwd = 2, col = col.dif[unique(temp$Grupo)], legend = groups[1:length(groups) %in% unique(temp$Grupo)], box.lty = 0, text.font = 2)
      if(!is.null(density)){
        if(density == "area"){
          plot.new()
          vps = baseViewports()
          pushViewport(vps$figure)
          vp1 = plotViewport(c(-1, 3.4, 1, 1.5))
          g = ggplot(habs[habs$Grupo %in% temp$Grupo, ], aes(SCO, fill = Grupo)) + geom_density(alpha = 0.2) + ggtitle(main[i]) + labs(x = NULL, y = NULL) + xlim(c(xlim[1], xlim[2])) + scale_y_continuous(breaks = NULL) + scale_fill_manual(breaks = unique(temp$Grupo), values = col.dif[unique(temp$Grupo)]) +
            theme(legend.position = "none", panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA), axis.text.x = element_blank(), plot.title = element_text(hjust = .5))
          print(g, vp = vp1)
        }
        if(density == "points"){
          par(mar = c(0, 4.5, 2, 1.5), bty = "n")
          plot(temp$POINT[1:nqp], seq(0, max(unlist(lapply(habs, FUN = function(x) x[[2]]))), length = nqp), type = "n", main = main[i], xaxt = "n", yaxt = "n", ann = FALSE)
          for(i in seq_along(habs)[seq_along(habs) %in% temp$Grupo]){
            info = cbind(habs[[i]][["x"]], habs[[i]][["y"]]); info = info[seq(1, nrow(info), 4), ]
            points(info[, 1], info[, 2], col = col.dif[i], pch = 16, cex = .5)
          }
        }
      }
      if(is.null(shinyDemo)){
        dev.off()
      }else{
        break
      }
    }
  }