#' @title Gera os graficos da CCI
#' @name cci.plot
#' 
#' @description  Cria um diretorio chamado 'CCI' e gera os graficos da Curva Caracteristica do Item (CCI).
#'
#' @param pars DF com os parametros dos itens. Deve ser composto com os seguintes campos: (Codigo, a, b, c).
#' @param main String ou vetor com o(s) titulo(s) dos graficos.
#' @param SCO Vetor de proficiencias para o plot da densidade.
#' @param breaks Limite inferior e superior dos pontos de quadratura, acompanhados pelo step da sequencia.
#' @param files Vetor com o nome dos arquivos. Caso NULL, serao utilizados os nomes dos itens no objeto 'dados'.
#' @param dir.create Nome do diretorio a ser criado para guardar os graficos.
#' @param height Altura em polegadas da imagem.
#' @param width Largura em polegadas da imagem.
#' @param shinyDemo Argumento auxiliar para o uso da funcao 'myR.app'.
#'
#' @details Etapa Anterior: 'read.SCO'.
#'
#' @author Brunno Bittencourt
#' 
#' @examples 
#' pars = data.frame(Codigo = 1001:1006, a = abs(rnorm(6, mean = 1.5, sd = 0.5)), b = rnorm(6, mean = 0.5), c = runif(6, 0.1, 0.3))
#'				  
#' cci.plot(pars, paste0("Item ", pars$Codigo))
#' 
#' @export
cci.plot <-
  function(pars, main = "CCI PLOT", SCO = NULL, breaks = c(-4, 4, 0.1), files = NULL, dir.create = "CCI", width = 9.6, height = 6.8, shinyDemo = NULL){
    if(sum(names(pars) %in% c("Codigo", "a", "b", "c")) < 4)
      stop("Os nomes das variaveis no arquivo de parametros devem seguite a seguinte nomenclatura: (Codigo, a, b, c, a01), sendo este ultimo opcional para o plot da curva de informação.")
    points = seq(breaks[1], breaks[2], breaks[3])
    if(length(main) == 1) main %<>% rep(nrow(pars))
    if(is.null(shinyDemo))
      dir.create(dir.create, showWarnings = FALSE)
    for(it in 1:nrow(pars)){
      if(is.null(shinyDemo)){
        pdf(file = paste0(getwd(), "/",dir.create,"/", paste0(ifelse(is.null(files), pars$Codigo[it], files[it]), "_CCI.pdf")), width = width, height = height)
      }else{
        it = shinyDemo
      }
      if(!is.null(SCO)){
        layout(matrix(c(2, 1), ncol = 1, byrow = TRUE), widths = c(5/7, 2/7), heights = c(2/7, 5/7))
        par(mar = c(4, 4, 1, 1), mai = c(1.02, 0.82, 0.02, 0.42))
      }
      infos = data.frame(Hab = points, Prob = rep(NA, length(points)), Info = rep(NA, length(points)))
      infos$Prob = pars$c[it] + ((1 - pars$c[it]) / (1 + exp(-1.7 * pars$a[it] * (infos$Hab - pars$b[it]))))
      if(any(names(pars) == "a01"))
        infos$Info = (pars$a01[it] ^ 2) * (1 - infos$Prob) * (((infos$Prob - pars$c[it]) / (1 - pars$c[it])) ^ 2) / infos$Prob
      plot(infos$Hab, infos$Prob, type = "n", xlim = c(breaks[1], breaks[2]), ylim = seq(0, 1), xlab = "Proficiência", ylab = "Probabilidade", main = ifelse(!is.null(SCO), "", main[it]))
      with(infos, lines(Hab, Prob, lwd = 2, col = "black"))
      with(infos, lines(Hab, Info, lwd = 2, col = "red"))
      abline(h = 0.65, lty = 2)
      
      if(!is.null(SCO)){  
        xhist = hist(SCO, plot = F, breaks = seq(from = min(infos$Hab), to = max(infos$Hab), length.out = 20))
        xx = seq(min(xhist$breaks), max(xhist$breaks), length.out = 100)
        xy = dnorm(xx, mean(SCO), sd(SCO))
        par(mar = c(0, 3.5, 3, 1))
        barplot(xhist$density, axes = FALSE, ylim = c(0, max(xhist$density, xy)), space = 0, col = "gray87", main = main[it])
        lines(seq(0, length(xhist$breaks) - 1, length.out = length(xy)), xy, col = "black")
      }
      if(is.null(shinyDemo)){
        dev.off()
      }else{
        break
      }
    }
  }