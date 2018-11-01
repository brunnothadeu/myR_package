#' @title Gera os graficos de AGI
#' @name agi.plot
#' 
#' @description Cria um diretorio chamado 'AGI' e gera os graficos de Analise Grafica do Item (AGI).
#' 
#' @param dados Banco de dados a ser utilizado. Este deve ser composto com os seguintes campos: (IT1, ..., ITm).
#' @param desemp Medida de desempenho a ser utilizada.
#' @param redGab Vetor de gabaritos. Caso este argumento seja passado a funcao, as curvas referentes as marcacoes corretas serao printadas com a cor vermelha.
#' @param main String ou vetor com o(s) titulo(s) dos graficos.
#' @param files Vetor com o nome dos arquivos. Caso NULL, serao utilizados os nomes dos itens no objeto 'dados'.
#' @param xlab Label do eixo x.
#' @param Alts Vetor com as possiveis marcacoes presentes nos dados.
#' @param col.agi Vetor com as coloracoes a serem utilizadas nas curvas. A ordem das cores sera equivalente a ordem no objeto 'Alts'. Caso redGab, a coloracao do gabarito sera ignorada. Caso NULL, coloracoes aleatorias serao utilizadas.
#' @param cex Vetor com as dimensoes dos plots de 'Alts' perante as curvas. A ordem dos elementos sera equivalente a ordem de 'Alts' e 'col'.
#' @param density Plotar na imagem a distribuicao dos escores.
#' @param smooth Suavizar as curvas de comportamento, interpolando 'smooth' pontos na curva.
#' @param dir.create Nome do diretorio a ser criado para guardar os graficos.
#' @param xlim Limites do eixo X a ser plotados.
#' @param height Altura em polegadas da imagem.
#' @param width Largura em polegadas da imagem.
#' @param shinyDemo Argumento auxiliar para o uso da funcao 'myR.app'.
#'
#' @details Etapa Anterior: 'escore'.
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
#' agi.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr$Escore, redGab = c("A", "B", "A", "C", "A", "B"), main = paste0("AGI ", names(dados)[-c(1:3, ncol(dados))]), Alts = c("A", "B", "C"))
#' 
#' @import magrittr
#' @importFrom randomcoloR distinctColorPalette
#' @export
agi.plot <-
  function(dados, desemp, redGab = NULL, main = "AGI PLOT", files = NULL, xlab = "Escore", Alts = c(".", "*", "A", "B", "C", "D", "E"), col.agi = NULL, cex = c(3, 1.2, rep(0.7, 5)), density = TRUE, smooth = NULL, dir.create = "AGI", xlim = NULL, width = 9.6, height = 6.8, shinyDemo = NULL){
    if(length(unique(desemp)) > 40){
      desemp %<>% cut_interval(40) %>% as.character %>% substr(., 2, nchar(.)-1) %>% strsplit(",") %>% lapply(FUN = function(x) mean(as.numeric(x))) %>% unlist
      if(xlab == "Escore")
		xlab = "Proficiencia"
      is.prof = TRUE
    }else{
      is.prof = FALSE
    }
    if(is.null(col.agi))
      col.agi = distinctColorPalette(length(Alts))
    if(is.null(xlim))
      xlim = c(min(desemp), max(desemp))
    if(length(main) == 1) main %<>% rep(ncol(dados))
    if(is.null(shinyDemo))
      dir.create(dir.create, showWarnings = FALSE)
    for(i in seq_along(dados)){
      if(is.null(shinyDemo)){
        pdf(file = paste0(getwd(), "/",dir.create,"/", paste0(ifelse(is.null(files), names(dados)[i], files[i]), "_AGI.pdf")), width = width, height = height)
      }else{
        i = shinyDemo
      }
      freq = prop.table(table(desemp, dados[[i]]), margin = 1)
      freq[is.na(freq)] = 0
      xVal = freq %>% row.names %>% as.numeric
      if(density){
        layout(matrix(c(2, 1), ncol = 1, byrow = TRUE), widths = c(5/7, 2/7), heights = c(2/7, 5/7))
        par(mar = c(4, 4, 1, 1), mai = c(1.02, 0.82, 0.02, 0.42), bty = "o")
      }
      plot(xVal, seq(0, 1, length = length(xVal)), type = "n", xlab = xlab, ylab = "Percentual de Respostas", main = ifelse(density, "", main[i]), xlim = xlim)
      for(alt in colnames(freq)){
        altCol = ifelse(is.null(redGab), col.agi[which(Alts == alt)], ifelse(alt == redGab[i], "red", col.agi[which(Alts == alt)]))
        if(is.null(smooth)){
          lines(xVal, freq[, alt], col = altCol, lwd = 2)
        }else{
          info = spline(xVal, freq[, alt], n = nrow(freq) * smooth)
          info$y[info$y < 0] = 0; info$y[info$y > 1] = 1
          lines(info[[1]], info[[2]], col = altCol, lwd = 2)
        }
        points(xVal, freq[, alt], col = altCol, pch = alt, cex = cex[which(Alts == alt)], font = 2 )
      }
      if(density){
        xhist = hist(desemp, plot = FALSE, breaks = seq(from = xlim[1], to = max(desemp), length.out = max(desemp) + 1))
        xx = seq(xlim[1], xlim[2], length.out = length(unique(desemp)) * 5)
        xy = dnorm(xx, mean(desemp), sd(desemp))
        par(mar = c(0, 4.1, 2, 2.1), bty = "n")
        plot(xVal, seq(0, max(xhist$density, xy), length = length(xVal)), type = "n", xlab = "", ylab = "", main = ifelse(density, "", main[i]), xlim = xlim, xaxt = "n", yaxt = "n")
        barplot(xhist$density, axes = F, ylim = c(0, max(xhist$density, xy)), space = 0, col = "gray87", main = main[i], add = T)
        if(is.prof){
          lines(seq_along(xhist$density), dnorm(seq(min(desemp), max(desemp), length.out = length(xhist$density))), col = "black")
        }else{
          lines(xx, xy, col = "black")
        }
      }
      if(is.null(shinyDemo)){
        dev.off()
      }else{
        break
      }
    }
  }