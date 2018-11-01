#' @title Gera os graficos de AGI, CCI e DIF
#' @name all.plot
#' 
#' @description Cria um diretorio chamado 'ALL PLOT' e gera os graficos dos item conjuntamente.
#' 
#' @param dados Banco de dados a ser utilizado na AGI. Este deve ser composto com os seguintes campos: (IT1, ..., ITm).
#' @param desemp Medida de desempenho a ser utilizada na AGI.
#' @param pars DF com os parametros dos itens a serem utilizados para a CCI. Deve ser composto com os seguintes campos: (Codigo, a, b, c).
#' @param EXP Objeto resultante da funcao calcDIF.
#' @param SCO Vetor de proficiencias para o plot da densidade.dddddddddddddddd
#' @param xlab Vetor de labels para os eixos x dos tres graficos.
#' @param ylab Vetor de labels para os eixos y dos tres graficos.
#' @param main String ou vetor com o(s) titulo(s) dos graficos.
#' @param files Vetor com o nome dos arquivos. Caso NULL, serao utilizados os nomes dos itens no objeto 'pars'.
#' @param redGab Vetor de gabaritos. Caso este argumento seja passado a funcao, as curvas referentes as marcacoes corretas na AGI serao printadas com a cor vermelha.
#' @param xlim.agi Limites do eixo X a ser plotados na AGI.
#' @param smooth Suavizar as curvas da AGI interpolando a curva atraves de 'nCat * smooth' pontos na curva, sendo nCat o numero de possiveis escores.
#' @param Alts Vetor com as possiveis marcacoes presentes nos dados.
#' @param col.agi Vetor com as coloracoes a serem utilizadas nas curvas. A ordem das cores sera equivalente a ordem no objeto 'Alts'. Caso redGab, a coloracao do gabarito sera ignorada. Caso NULL, coloracoes aleatorias serao utilizadas.
#' @param cex Vetor com as dimensoes dos plots de 'Alts' perante as curvas da AGI. A ordem dos elementos sera equivalente a ordem de 'Alts' e 'col.agi'.
#' @param breaks Limite inferior e superior dos pontos de quadratura na CCI, acompanhados pelo step da sequencia.
#' @param groups Vetor com o nome a ser plotado para cada um dos grupos nos graficos de DIF, ordenado pelo indice do respectivo grupo.
#' @param probs Quantiles utilizados para o calculo dos intervalos de interesse na analise de DIF.
#' @param col.dif Vetor com as cores a serem utilizadas nas curvas empiricas dos grupos nos graficos de DIF.
#' @param xlim.dif Limites do eixo X a ser plotados nos graficos de DIF.
#' @param dir.create Nome do diretorio a ser criado para guardar os graficos.
#' @param width Largura em polegadas da imagem.
#' @param height Altura em polegadas da imagem.
#' @param shinyDemo Argumento auxiliar para o uso da funcao 'myR.app'.
#'
#' @details Etapa Anterior: 'escore', 'checkDIF', 'read.SCO'.
#' @details Os objetos 'dados', 'pars' e 'EXP' devem estar na mesma ordenacao em relacao aos itens. Isto e, a quinta coluna de 'dados' deve ser referente a quinta linha de 'pars', que por sua vez e equivalente ao quinto elemento de 'EXP'.
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
#' pars = data.frame(Codigo = 1001:1006, a = abs(rnorm(6, mean = 1.5, sd = 0.5)), b = rnorm(6, mean = 0.5), c = runif(6, 0.1, 0.3))
#' 
#' EXP = remakeEXP(readLines("EXPtest.EXP"))
#'
#' newGroup = c(6, 7)
#'
#' EXP = calcDIF(EXP, newGroup)
#'
#' SCO = read.SCO(readLines("SCOtest.SCO"))
#'
#' #Selecionando itens do teste.
#' EXP = EXP[names(EXP) %in% c("IT23094", "IT23095", "IT20345", "IT23094", "IT34909", "IT23094")
#'
#' all.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr$Escore, redGab = c("A", "B", "A", "C", "A", "B"), xlim.agi = c(0, 6), smooth = 5, pars = pars, SCO = SCO, EXP = EXP)
#' 
#' @import magrittr
#' @importFrom randomcoloR distinctColorPalette
#' @export
all.plot <- 
  function(dados, desemp, pars, EXP, SCO, xlab = c("Escore", "Proficiencia", "Proficiencia"), ylab = c("Percentual de Respostas", "Probabilidade", "Percentual de Respostas"), main = "ALL PLOT",
           files = NULL, redGab = NULL, xlim.agi = NULL, smooth = NULL, Alts = c(".", "*", "A", "B", "C", "D", "E"), col.agi = NULL, cex = c(3, 1.2, rep(0.7, 5)),
           breaks = c(-4, 4, 0.1), groups = NULL, probs = c(0.05, 0.95), col.dif = NULL, xlim.dif = c(-4, 4), dir.create = "Full Plot", width = 9.6, height = 6.8, shinyDemo = NULL){
    if(ncol(dados) != length(EXP) | ncol(dados) != nrow(pars))
      stop("O numero de colunas em dados nao confere com o numero de linhas em pars e extensao em EXP")
    if(length(main) == 1) main %<>% rep(ncol(dados))
    if(length(unique(desemp)) > 50){
      desemp %<>% cut_interval(30) %>% as.character %>% substr(., 2, nchar(.)-1) %>% strsplit(",") %>% lapply(FUN = function(x) mean(as.numeric(x))) %>% unlist
      if(xlab[1] == "Escore") xlab[1] = "Proficiencia"
    }
    if(is.null(col.agi))
      col.agi = distinctColorPalette(length(Alts))
    if(is.null(xlim))
      xlim = c(min(desemp), max(desemp))
    points = seq(breaks[1], breaks[2], breaks[3])
    if(is.null(groups))
      groups = paste0("Grupo", unique(SCO$Grupo))
    if(is.null(col.dif))
      col.dif = distinctColorPalette(length(groups))
    SCO = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(probs[1], probs[2]))
    if(is.null(shinyDemo))
      dir.create(dir.create, showWarnings = FALSE)
    for(i in seq_along(EXP)){
      if(is.null(shinyDemo)){
        pdf(file = paste0(getwd(), "/",dir.create,"/", paste0(ifelse(is.null(files), pars$Codigo[i], files[i]), "_PLOT.pdf")), width = width, height = height, paper = "USr")
      }else{
        i = shinyDemo
      }
      layout(matrix(c(1,1,2,2,0,3,3,0), ncol = 4, byrow = TRUE))
      par(mar = c(1, 1.5, 1, .5))
      par(fig = c(.01, .48, .55, .96))
      freq = prop.table(table(desemp, dados[[i]]), margin = 1)
      freq[is.na(freq)] = 0
      xVal = freq %>% row.names %>% as.numeric
      plot(xVal, seq(0, 1, length = length(xVal)), type = "n", xlab = xlab[1], ylab = ylab[1], main = "", xlim = xlim.agi)
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
      par(fig = c(.52, .99, .55, .96), new = T) 
      infos = data.frame(Hab = points, Prob = rep(NA, length(points)), Info = rep(NA, length(points)))
      infos$Prob = pars$c[i] + ((1 - pars$c[i]) / (1 + exp(-1.7 * pars$a[i] * (infos$Hab - pars$b[i]))))
      if(any(names(pars) == "a01"))
        infos$Info = (pars$a01[i] ^ 2) * (1 - infos$Prob) * (((infos$Prob - pars$c[i]) / (1 - pars$c[i])) ^ 2) / infos$Prob
      plot(infos$Hab, infos$Prob, type = "n", xlim = c(breaks[1], breaks[2]), ylim = seq(0, 1), xlab = xlab[2], ylab = ylab[2], main = "")
      with(infos, lines(Hab, Prob, lwd = 2, col = "black"))
      with(infos, lines(Hab, Info, lwd = 2, col = "red"))
      abline(h = 0.65, lty = 2)
      par(fig = c(.265, .735, .04, .45), new = T)  
      temp = EXP[[i]]
      nqp = nrow(temp) / (temp$Grupo %>% unique %>% length)
      plot(temp$POINT[1:nqp], seq(0, 1, length = nqp), type = "n", main = main[i], xlab = xlab[3], ylab = ylab[3], ylim = c(0, 1), xlim = c(xlim.dif[1], xlim.dif[2]))
      lines(temp$POINT[1:nqp], temp$MODEL.PROP[1:nqp], lwd = 2)
      for(g in unique(temp$Grupo)){
        lines(temp$POINT[temp$Grupo == g], temp$PROPORTION[temp$Grupo == g], col = col.dif[g], lwd = 2)
        abline(v = SCO[[g]][1], lty = 2)
        abline(v = SCO[[g]][2], lty = 2)
      }
      legend(-4.2, 1, lty = 1, lwd = 2, col = col.dif[unique(temp$Grupo)], legend = groups[1:length(groups) %in% unique(temp$Grupo)], box.lty = 0, text.font = 2)
      if(is.null(shinyDemo)){
        dev.off()
      }else{
        break
      }
    }
  }