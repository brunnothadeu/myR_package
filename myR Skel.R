loadpkg <-
  function(){
    #Local Dev. Function
    library(magrittr)
    library(dplyr)
    library(SDMTools)
    library(gridBase)
    library(ggplot2)
    library(randomcoloR)
    library(shiny)
    library(shinydashboard)
    library(DT)
	  library(mirt)
    library(psych)
  }

vetor.estendido <-
  function(dados, bib, b.len){
    if(sum(dados$CAD %in% names(bib)) == 0)
      stop("Os valores de Cadernos entre o banco e o BIB não conferem.")
    nblocos = bib %>% unlist %>% unique %>% length
    aux = data.frame(matrix(NA, nrow = nrow(dados), ncol = b.len * nblocos))
    for(cad in names(bib)){
      ind =  bib[[cad]] %>% subtract(1) %>% multiply_by(b.len - 1) %>% add(bib[[cad]])
      ind = unlist(lapply(ind, FUN = function(x, b.len){seq(x, x + b.len - 1)}, b.len = b.len))
      aux[dados$CAD == cad, ind] = dados[dados$CAD == cad, 4:ncol(dados)]
    }
    aux = cbind(dados[1:3], aux); names(aux) = c("ID", "CAD", "PESO", paste0("IT", 1:(nblocos * b.len)))
    return(aux)
  }

vetor.estendido2 <-
  function(dados, configCADS){
    CodIts = configCADS %>% unlist %>% unique
    CodIts = CodIts[order(CodIts, decreasing = F)]
    aux = data.frame(matrix(NA, nrow = nrow(dados), ncol = length(CodIts)))
    names(aux) = paste0("IT_", CodIts)
    for(cad in names(configCADS))
      aux[dados$CAD == cad, paste0("IT_", configCADS[[cad]])] = dados[dados$CAD == cad, 4:ncol(dados)]
    aux = cbind(dados[1:3], aux)
    return(aux)
  }

corrigir <-
  function(dados, gab){
    dadosCorr = cbind(dados[1:3], key2binary(dados[-c(1:3)], gab))
    names(dadosCorr) = names(dados)
    return(dadosCorr)
  }

escore <-
  function(dadosCorr, method = "Escore"){
    escore <- dadosCorr[4:ncol(dadosCorr)] %>% rowSums(na.rm = T)
    if(tolower(method) == "escore"){
      dadosCorr$Escore <- escore
      return(dadosCorr)
    }
    if(tolower(method) == "normit"){
      Escores <- list(Freq = dadosCorr$PESO) %>% aggregate(list(escore = escore), FUN = sum)
      Escores$Freq <- Escores$Freq %>% cumsum
      normit <- dadosCorr %>% nrow %>% numeric
      normit[escore == Escores$escore[1]] = qnorm((Escores$Freq[1] + (Escores$Freq[2] / 2)) / (2 * sum(dadosCorr$PESO)))
      for(j in 2:nrow(Escores))
        normit[escore == Escores$escore[j]] = qnorm((Escores$Freq[j] + Escores$Freq[j-1]) / (2 * sum(dadosCorr$PESO)))
      dadosCorr$Normit <- normit
      return(dadosCorr)
    }  
  }

tct <-
  function(dados, dadosCorr, mapa, Alts = c("A", "B", "C", "D", "E", "*", "."), pbis = FALSE, itemResto = FALSE, arred = 10, sdpop = FALSE, crit = c(0.16, 0.1, 0), summary = FALSE){
    TCT <- matrix(NA, nrow = ncol(dados) - 4, ncol = ncol(mapa) + 6 + length(Alts) * 2) %>% data.frame
    names(TCT) <- c(names(mapa), "Dif", "Disc", "Abai", "Acim", "Bis", paste0(rep(c("Perc", "Bis"), each = length(Alts)), Alts), "Status")
    TCT[1:ncol(mapa)] <- mapa
    names(dadosCorr)[ncol(dadosCorr)] <- names(dados)[ncol(dados)] <- "Desempenho"
    if(summary){
      summ <- data.frame(Info = c("Número total de respondentes", "Número de respondentes por item", "Desvio-Padrão por item", "Desempenho médio do grupo gabarito",
                                 "Desempenho médio do grupo não-gabarito","Quantile .27 do gabarito","Quantile .73 do gabarito"))
      summ <- summ %>% cbind(matrix(NA, nrow = nrow(summ), ncol = nrow(mapa)))
      names(summ) <- c("Info", paste0("IT", 1:nrow(mapa)))
    }
    desemp <- dados$Desempenho
    for(it in 1:nrow(TCT)){
      respOn = dadosCorr[[it + 3]] %>% is.na %>% not
      if(!isFALSE(itemResto)){
        desemp <- rowSums(dadosCorr[-c(1:3, ncol(dadosCorr))], na.rm = T) - dadosCorr[[it + 3]]
        if(tolower(itemResto) == "normit"){
          Escores <- list(Freq = dadosCorr$PESO) %>% aggregate(list(escore = desemp), FUN = sum)
          Escores$Freq <- Escores$Freq %>% cumsum
          normit <- dadosCorr %>% nrow %>% numeric
          normit[desemp == Escores$escore[1]] = qnorm((Escores$Freq[1] + (Escores$Freq[2] / 2)) / (2 * sum(dadosCorr$PESO)))
          for(j in 2:nrow(Escores))
            normit[desemp == Escores$escore[j]] = qnorm((Escores$Freq[j] + Escores$Freq[j-1]) / (2 * sum(dadosCorr$PESO)))
          desemp <- normit
        }
      }
      if(sdpop){
        sd <- sqrt(sum(dados$PESO[respOn] * ((desemp[respOn] - weighted.mean(desemp[respOn], dados$PESO[respOn]))^2)) / sum(dados$PESO[respOn]))
      }else{
        sd <- wt.sd(desemp[respOn], dados$PESO[respOn])
      }
      if(summary){
        summ[1, it + 1] <- nrow(dadosCorr)
        summ[2, it + 1] <- sum(respOn)
        summ[3, it + 1] <- sd
      }
      u <- wt.mean(desemp[respOn], dados$PESO[respOn])
      for(alt in Alts){
        respAlt <- respOn & (dados[[it + 3]] == alt)
        pAlt <- wt.mean(dados[respOn, it + 3] == alt, dadosCorr$PESO[respOn])
        TCT[it, paste0("Perc", alt)] <- pAlt %>% round(arred)
        u1 <- wt.mean(desemp[respAlt], dadosCorr$PESO[respAlt])
        if(!pbis){
          z <- pAlt %>% qnorm %>% dnorm
          u0 <- wt.mean(desemp[!respAlt & respOn], dadosCorr$PESO[!respAlt & respOn])
          TCT[it, paste0("Bis", alt)] <- ((pAlt * (1 - pAlt) * (u1 - u0)) / (sd * z)) %>% round(arred)
        }else{
          TCT[it, paste0("Bis", alt)] <- (((u1 - u) / sd) * sqrt(pAlt / (1 - pAlt))) %>% round(arred)
        }
        if(alt == TCT$Gabarito[it]){
          quantile <- desemp[respOn] %>% quantile(probs = c(.27, .73), type = 6)
          grupo <- ifelse(desemp <= quantile[1], 1, ifelse(desemp >= quantile[2], 3, 2))
          TCT$Abai[it] <- dadosCorr[respOn & grupo == 1, it + 3] %>% wt.mean(dadosCorr$PESO[respOn & grupo == 1]) %>% round(arred)
          TCT$Acim[it] <- dadosCorr[respOn & grupo == 3, it + 3] %>% wt.mean(dadosCorr$PESO[respOn & grupo == 3]) %>% round(arred)
          TCT$Dif[it] <- TCT[it, paste0("Perc", alt)]
          TCT$Bis[it] <- TCT[it, paste0("Bis", alt)] 
          if(summary){
            summ[4, it + 1] <- u1
            summ[5, it + 1] <- u0  
            summ[6, it + 1] <- quantile[1]
            summ[7, it + 1] <- quantile[2]
          }
        }
      }
      bisDist <- TCT[it, paste0("Bis", Alts)][paste0("Bis", Alts) != paste0("Bis", TCT$Gabarito[it])]
      if(!is.na(TCT$Bis[it]) & (TCT$Bis[it] < crit[1] | any(bisDist > crit[2], na.rm = T) | any((TCT$Bis[it] - bisDist) < crit[3], na.rm = T)))
        TCT$Status[it] <- "CheckBiss"
    }
    TCT$Disc <- (TCT$Acim - TCT$Abai) %>% round(arred)
    if(pbis)
      names(TCT)[substr(names(TCT), 1, 3) == "Bis"] <- paste0("P", names(TCT)[substr(names(TCT), 1, 3) == "Bis"])
    hSD <- as.vector(sapply(dadosCorr[-c(1:3, ncol(dadosCorr))], sd, na.rm = T)) > 0
    alphaResto <- dadosCorr[c(rep(F, 3), hSD, F)] %>% alpha
    TCT$Alpha <- NA
    TCT$Alpha[hSD] <- ifelse((alphaResto[[1]][[1]] - alphaResto[[2]][, 1]) >= 0, "+", "-") %>% paste0(" (", round(abs(alphaResto[[1]][[1]] - alphaResto[[2]][, 1]), arred), ")")
    if(summary)
      TCT <- list(tct = TCT, summary = summ)
    return(TCT)
  }

agi.plot <-
  function(dados, desemp, redGab = NULL, main = "AGI PLOT", files = NULL, xlab = "Escore", ylab = "Percentual de Respostas", Alts = c(".", "*", "A", "B", "C", "D", "E"), col.agi = NULL, cex = c(3, 1.2, rep(0.7, 5)), density = TRUE, smooth = NULL, dir.create = "AGI", xlim = NULL, width = 9.6, height = 6.8, ext = "PDF", shinyDemo = NULL){
    if(length(unique(desemp)) > 45){
      desemp %<>% cut_interval(45) %>% as.character %>% substr(., 2, nchar(.)-1) %>% strsplit(",") %>% lapply(FUN = function(x) mean(as.numeric(x))) %>% unlist
      if(xlab == "Escore") xlab = "Proficiência"
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
        if(ext == "PNG"){
          png(file = paste0(getwd(), "/",dir.create,"/", paste0(ifelse(is.null(files), names(dados)[i], files[i]), ".png")), width = width, height = height, res = 100)
        }else{
          pdf(file = paste0(getwd(), "/",dir.create,"/", paste0(ifelse(is.null(files), names(dados)[i], files[i]), ".pdf")), width = width, height = height)
        }
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
      plot(xVal, seq(0, 1, length = length(xVal)), type = "n", xlab = xlab, ylab = ylab, main = ifelse(density, "", main[i]), xlim = xlim)
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

pos.prop <-
  function(dadosCorr, desemp, PESO = NULL, breaks = c(-5, 5, 0.1), crit = list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0)){
    comp = data.frame(Name = c("PROP", "NRESP", "PROP2", "DIF"), Value = as.vector(unlist(list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0))), stringsAsFactors = F) %>%
      anti_join(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F), by = "Name") %>%
      rbind(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F))
    crit = comp$Value %>% split(comp$Name)  
    if(is.null(PESO)) PESO = rep(1, length(desemp))
    desemp = cut(desemp, breaks = c(seq(breaks[1], breaks[2], breaks[3]) - breaks[3] / 2, breaks[2] + breaks[3] / 2), 
                 labels = seq(breaks[1], breaks[2], breaks[3]))
    POS = data.frame(matrix(NA, nrow = ncol(dadosCorr), ncol = length(levels(desemp)))); names(POS) = levels(desemp)
    for(it in seq_along(dadosCorr))
      POS[it, ] = data.frame(IT = dadosCorr[[it]], PESO = PESO) %>% split(desemp) %>% lapply(FUN = function(x) wt.mean(x[, 1], x[, 2])) %>% unlist
    pos = rep(NA, nrow(POS))
    POS[is.na(POS)] = 0
    for(it in 1:nrow(POS)){
      for(p in names(POS)){
        if(is.na(POS[it, p]))
          next
        if(sum(!is.na(dadosCorr[[it]][desemp == p])) >= crit$NRESP & POS[it, p] >= crit$PROP & 
           ifelse(which(names(POS) == p) > 1, POS[it, (which(names(POS) == p) - 1)] < crit$PROP2, FALSE) &
           ifelse(which(names(POS) == p) > 1, (POS[it, p] - POS[it, (which(names(POS) == p) - 1)]) >= crit$DIF, TRUE)){
          pos[it] = p
          break
        }
      }
    }
    return(pos)
  }

pos.model <-
  function(pars, breaks = c(-5, 5, 0.1), crit = list(PROP = 0.65, PROP2 = 1, DIF = 0)){
    comp = data.frame(Name = c("PROP", "NRESP", "PROP2", "DIF"), Value = as.vector(unlist(list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0))), stringsAsFactors = F) %>%
      anti_join(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F), by = "Name") %>%
      rbind(data.frame(Name = names(crit), Value = as.vector(unlist(crit)), stringsAsFactors = F))
    crit = comp$Value %>% split(comp$Name)  
    points = seq(breaks[1], breaks[2], breaks[3])
    POS = data.frame(matrix(NA, nrow = nrow(pars), ncol = length(points))); names(POS) = as.character(round(points, 1))
    for(it in 1:nrow(pars))
      POS[it, ] = pars$c[it] + (1 - pars$c[it]) / (1 + exp(-1.7 * pars$a[it] * (points - pars$b[it])))
    pos = rep(NA, nrow(POS))
    for(it in 1:nrow(POS)){
      for(p in names(POS)){
        if(POS[it, p] >= crit$PROP & 
           ifelse(which(names(POS) == p) > 1, POS[it, as.character(as.numeric(p) - breaks[3])] < crit$PROP2, FALSE) &
           ifelse(which(names(POS) == p) > 1, (POS[it, p] - POS[it, as.character(as.numeric(p) - breaks[3])]) >= crit$DIF, TRUE)){
          pos[it] = p
          break
        }
      }
    }
    return(pos)
  }

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

read.SCO <-
  function(SCO, summary = TRUE, getID = FALSE){
    if(length(SCO) == 1)
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

remakeEXP <-
  function(EXP){
    if(length(EXP) == 1)
      EXP <- readLines(paste0(EXP, ".EXP"))
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
    Ajuste = data.frame(matrix(Ajuste[-1, ], ncol = 4, byrow = FALSE), stringsAsFactors = F); names(Ajuste) = c("Item", "Grupo", "Ponto", "DIF")
    Ajuste$Ponto <- as.numeric(Ajuste$Ponto); Ajuste$DIF <- as.numeric(Ajuste$DIF)
    return(list(DIF = unique(DIF), Ajuste = unique(Ajuste)))
  }

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
      stop("Dimensões inválidas para main")
    if(!is.null(density)){
      habs = SCO
      if(density == "points"){
        habs = split(habs$SCO, habs$Grupo)
        for(i in seq_along(habs))
          habs[[i]] = density(habs[[i]])
      }
    }
    SCO = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(probs[1], probs[2]))
    if(is.null(files))
      files <- names(EXP)
    for(i in seq_along(EXP)){
      if(is.null(shinyDemo)){
        pdf(file = paste0(getwd(), "/", dir.create,"/", paste0(files[i], ".pdf")), width = width, height = height)
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
      plot(temp$POINT[1:nqp], seq(0, 1, length = nqp), type = "n", main = ifelse(is.null(density), main[i], ""), xlab = "Proficiência", ylab = "Percentual de Respostas", ylim = c(0, 1), xlim = c(xlim[1], xlim[2]))
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

all.plot <- 
  function(dados, desemp, pars, EXP, SCO, xlab = c("Escore", "Proficiência", "Proficiência"), ylab = c("Percentual de Respostas", "Probabilidade", "Percentual de Respostas"), main = "ALL PLOT",
           files = NULL, redGab = NULL, xlim.agi = NULL, smooth = NULL, Alts = c(".", "*", "A", "B", "C", "D", "E"), col.agi = NULL, cex = c(3, 1.2, rep(0.7, 5)),
           breaks = c(-4, 4, 0.1), groups = NULL, probs = c(0.05, 0.95), col.dif = NULL, xlim.dif = c(-4, 4), dir.create = "Full Plot", width = 9.6, height = 6.8, shinyDemo = NULL){
    if(ncol(dados) != length(EXP) | ncol(dados) != nrow(pars))
      stop("O número de colunas em dados não confere com o número de linhas em pars e extensão em EXP")
    if(length(main) == 1) main %<>% rep(ncol(dados))
    if(length(unique(desemp)) > 50){
      desemp %<>% cut_interval(30) %>% as.character %>% substr(., 2, nchar(.)-1) %>% strsplit(",") %>% lapply(FUN = function(x) mean(as.numeric(x))) %>% unlist
      if(xlab[1] == "Escore") xlab[1] = "Proficiência"
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

makedb <-
  function(dados){
    dados <- data.frame(ID = 1:nrow(dados), CAD = 1, PESO = 1, dados)
    dados$PESO %<>% as.numeric
    return(dados)
  }

openstring <-
  function(dados){
    dados %<>% strsplit("") %>% unlist %>% matrix(nrow = length(dados), byrow = TRUE) %>% data.frame(stringsAsFactors = FALSE)
    names(dados) = paste0("IT", 1:ncol(dados))
    return(dados)
  }

myR.app <-
  function(){
    ui <- dashboardPage(
      dashboardHeader(title = "Interface MyR"),
      
      dashboardSidebar(
        sidebarMenu(
          
          menuItem("Intrucoes", tabName = "introTab"),
          
          menuItem("Leitura", tabName = "leituraTab", 
                   menuItem("Definicoes", tabName = "leituraTabConfig",
                            menuSubItem(icon = NULL, checkboxInput('header', 'Cabecalho', TRUE)),
                            menuSubItem(icon = NULL, radioButtons('sep', 'Separator', c('Comma (,)' = ',', 'Semicolon (;)' = ';', 'Tab ( )' = '\t'), ';')),
                            menuSubItem(icon = NULL, radioButtons('quote', 'Quote', c('None' = '', 'Double Quote' = '"', 'Single Quote' = "'"), '')),
                            menuSubItem(icon = NULL, numericInput("nhead", "Linhas exibidas:", 10)),
                            menuSubItem(icon = NULL, numericInput("maxupload", "Limite para upload (em MB):", 50)),
                            menuSubItem(icon = NULL, actionButton("maxuploadButtom", "Aumentar Limite"))
                   ),
                   menuItem("Respostas", tabName = "leituraTabRespostas"),
                   menuItem("Mapa", tabName = "leituraTabMapa"),
                   menuItem("BIB", tabName = "leituraTabBIB"),
                   menuItem("Parametros", tabName = "leituraTabParametros"),
                   menuItem("Desempenho para Posicionar", tabName = "leituraTabDesempPos"),
                   menuItem(".SCO", tabName = "leituraTabSCO"),
                   menuItem(".EXP", tabName = "leituraTabEXP"),
                   menuItem("INAmes - Full Plot", tabName = "leituraTabINAmesFP")
          ),
          
          menuItem("Abertura", tabName = "aberturaTab",
                   menuItem("Definicoes", tabName = "aberturaTabConfig",
                            menuSubItem(icon = NULL, radioButtons('tipoVE', 'Metodo', c('BIB' = 'bib', 'Codigo dos Itens' = 'codit'), 'BIB')),
                            menuSubItem(icon = NULL, numericInput("b.len", "Numero de itens em cada bloco:", 10)),
                            menuSubItem(icon = NULL, numericInput("nheadVE", "Linhas exibidas:", 12))
                   ),
                   menuItem("Abrir banco", tabName = "aberturaTabAction")
          ),
          
          menuItem("Correcao", tabName = "correcaoTab",
                   menuItem("Definicoes", numericInput("Corrline", "Linha de exibicao:", 1)),
                   menuItem("Corrigir banco", tabName = "correcaoTabAction")
          ),
          
          menuItem("Escore", tabName = "escoreTab",
                   menuItem("Definicoes", radioButtons('tipoEsc', 'Metodo', c('Escore' = 'Escore', 'Normit' = 'Normit'), 'Escore')),
                   menuItem("Calcular Escore", tabName = "escoreTabAction")
          ),
          
          menuItem("AGI", tabName = "agiTab",
                   menuItem("Definicoes", tabName = "agiTabConfig",
                            menuSubItem(icon = NULL, textInput("agi.redGab", "Gabaritos", "NULL")),
                            menuSubItem(icon = NULL, textInput("agi.main", "Titulos", "Titulo IT1;Titulo IT2;Titulo IT3")),
                            menuSubItem(icon = NULL, textInput("agi.files", "Nomes dos arquivos PDF", "NULL")),                
                            menuSubItem(icon = NULL, textInput("agi.xlab", "Rotulo eixo X", "Escore")),                  
                            menuSubItem(icon = NULL, textInput("agi.Alts", "Marcacoes a serem consideradas", ".;*;A;B;C;D;E")),                  
                            menuSubItem(icon = NULL, textInput("agi.col", "Coloracao das curvas", "NULL")),                  
                            menuSubItem(icon = NULL, textInput("agi.cex", "Dimensao dos caracteres nas curvas", "3;1.2;0.7;0.7;0.7;0.7;0.7")), 
                            menuSubItem(icon = NULL, textInput("agi.density", "Plotar densidade", "FALSE")), 
                            menuSubItem(icon = NULL, textInput("agi.smooth", "Indice de suavizacao das curvas", "NULL")), 
                            menuSubItem(icon = NULL, textInput("agi.dir.create", "Nome do diretorio para salvar os arquivos", "AGI")), 
                            menuSubItem(icon = NULL, textInput("agi.xlim", "Limites do eixo X", "NULL")), 
                            menuSubItem(icon = NULL, numericInput("agi.width", "Largura em polegadas", 9.6)), 
                            menuSubItem(icon = NULL, numericInput("agi.height", "Altura em polegadas", 6.8))
                            
                   ),
                   menuItem("Gerar AGI", tabName = "agiTabAction")
          ),
          
          menuItem("TCT", tabName = "tctTab",
                   menuItem("Definicoes", tabName = "tctTabConfig",
                            menuSubItem(icon = NULL, textInput("tct.Alts", "Marcacoes a serem consideradas", ".;*;A;B;C;D;E")), 
                            menuSubItem(icon = NULL, numericInput("tct.arred", "Casas decimais a serem utilizadas:", 10)), 
                            checkboxInput("tct.sdpop", "Utilizar desvio populacional?", FALSE),
                            checkboxInput("tct.summary", "Calcular summary dos dados?", FALSE),
                            menuSubItem(icon = NULL, numericInput("nheadTCT", "Linhas exibidas:", 12))
                   ),
                   menuItem("Gerar TCT", tabName = "tctTabAction")
          ),
          
          menuItem("Posicionamento", tabName = "posTab",
                   menuItem("Definicoes", tabName = "posTabConfig",
                            menuSubItem(icon = NULL, selectInput("pos.method", "Metodo:", list("Modelo" = "model", "Empirico" = "prop"))),
                            menuSubItem(icon = NULL, textInput("pos.breaks", "Limites e step dos pontos de posicionamento", "-5;5;0.1")),  
                            menuSubItem(icon = NULL, numericInput("pos.PROP", "Proporcao mimina no nivel", 0.65)),
                            menuSubItem(icon = NULL, numericInput("pos.NRESP", "Numero minimo de respostas no nivel", 50)),
                            menuSubItem(icon = NULL, numericInput("pos.PROP2", "Proporcao maxima no nivel anterior", 1)),
                            menuSubItem(icon = NULL, numericInput("pos.DIF", "Diferenca minima entre P(i) e P(i-1)", 0))
                   ),
                   menuItem("Posicionar os Itens", tabName = "posTabAction")
          ),
          
          menuItem("CCI", tabName = "cciTab",
                   menuItem("Definicoes", tabName = "cciTabConfig",
                            menuSubItem(icon = NULL, checkboxInput('cci.info', 'Plotar curva de informacao', TRUE)),
                            menuSubItem(icon = NULL, textInput("cci.main", "Titulos", "Titulo IT1;Titulo IT2;Titulo IT3")),
                            menuSubItem(icon = NULL, textInput("cci.SCOg", "Grupos para plot da densidade", "NULL")),
                            menuSubItem(icon = NULL, textInput("cci.breaks", "Intervalo de proficiencias para o plot", "-5;5;0.1")), 
                            menuSubItem(icon = NULL, textInput("cci.files", "Nomes dos arquivos PDF", "NULL")),                
                            menuSubItem(icon = NULL, textInput("cci.dir.create", "Nome do diretorio para salvar os arquivos", "CCI")),
                            menuSubItem(icon = NULL, numericInput("cci.width", "Largura em polegadas", 9.6)), 
                            menuSubItem(icon = NULL, numericInput("cci.height", "Altura em polegadas", 6.8))
                   ),
                   menuItem("Gerar CCI", tabName = "cciTabAction")
          ),
          
          menuItem("DIF", tabName = "difTab",
                   menuItem("Definicoes", tabName = "difTabConfig",
                            
                            menuSubItem(icon = NULL, textInput("dif.newGroup", "Grupos de interesse", '1;2')),
                            menuSubItem(icon = NULL, numericInput("dif.crit", "Criterio de DIF", 0.15)),
                            menuSubItem(icon = NULL, textInput("dif.probs", "Intervalo de analise", "0.05;0.95")),
                            tags$hr(),
                            menuSubItem(icon = NULL, textInput("dif.main", "Titulos", "NULL")),
                            menuSubItem(icon = NULL, textInput("dif.groups", "Nome dos grupos", "NULL")),
                            menuSubItem(icon = NULL, textInput("dif.col", "Coloracao das curvas", "NULL")),     
                            menuSubItem(icon = NULL, textInput("dif.density", "Plotar densidade", "area")), 
                            menuSubItem(icon = NULL, textInput("dif.dir.create", "Nome do diretorio para salvar os arquivos", "DIF")),                  
                            menuSubItem(icon = NULL, textInput("dif.xlim", "Limites do eixo X", "-4;4")), 
                            menuSubItem(icon = NULL, numericInput("dif.width", "Largura em polegadas", 9.6)), 
                            menuSubItem(icon = NULL, numericInput("dif.height", "Altura em polegadas", 6.8))
                   ),
                   menuItem("Gerar DIF", tabName = "difTabAction")
          ),
          
          menuItem("ALL PLOT", tabName = "allTab",
                   menuItem("Definicoes", tabName = "allTabConfig",
                            menuSubItem(icon = NULL, textInput("all.xlab", "Rotulos eixo X", "Escore;Proficiencia;Proficiencia")),
                            menuSubItem(icon = NULL, textInput("all.ylab", "Rotulos eixo Y", "Percentual de Respostas;Probabilidade;Percentual de Respostas")),
                            menuSubItem(icon = NULL, textInput("all.main", "Titulos", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.files", "Nomes dos arquivos PDF", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.redGab", "Gabaritos", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.xlim.agi", "Limites do eixo X para a AGI", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.smooth", "Indice de suavizacao das curvas da AGI", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.Alts", "Marcacoes a serem consideradas", ".;*;A;B;C;D;E")),
                            menuSubItem(icon = NULL, textInput("all.col.agi", "Coloracao das curvas da AGI", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.cex", "Dimensao dos caracteres nas curvas da AGI", "3;1.2;0.7;0.7;0.7;0.7;0.7")),
                            menuSubItem(icon = NULL, checkboxInput('all.info.cci', 'Plotar curva de informacao na CCI', TRUE)),
                            menuSubItem(icon = NULL, textInput("all.breaks.cci", "Intervalo de proficiencias para o plot da CCI", "-4;4;0.1")),
                            menuSubItem(icon = NULL, textInput("all.groups", "Nome dos grupos para o DIF", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.probs", "Intervalo de analise do DIF", "0.05;0.95")),
                            menuSubItem(icon = NULL, textInput("all.col.dif", "Coloracao das curvas do DIF", "NULL")),
                            menuSubItem(icon = NULL, textInput("all.xlim.dif", "Limites do eixo X do DIF", "-4;4")),
                            menuSubItem(icon = NULL, textInput("all.dir.create", "Nome do diretorio para salvar os arquivos", "FULLPLOT")),
                            menuSubItem(icon = NULL, numericInput("all.width", "Largura em polegadas", 9.6)),
                            menuSubItem(icon = NULL, numericInput("all.height", "Altura em polegadas", 6.8))
                   ),
                   menuItem("Gerar DIF", tabName = "allTabAction")
          )
        )
      ),
      
      dashboardBody(
        tabItems(
          tabItem(tabName = "introTab",
                  verbatimTextOutput("Intro")
          ),        
          
          tabItem(tabName = "leituraTabRespostas",
                  fluidPage(
                    fluidRow(
                      fileInput('dados', 'Upload - Respostas',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headD", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabMapa",
                  fluidPage(
                    fluidRow(
                      fileInput('mapa', 'Upload - Mapa',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headM", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabBIB",
                  fluidPage(
                    fluidRow(
                      fileInput('bib', 'Upload - BIB',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headB", width = "150%")
                    )
                  )
          ),  
          
          tabItem(tabName = "leituraTabParametros",
                  fluidPage(
                    fluidRow(
                      fileInput('pars', 'Upload - Parametros',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headP", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabDesempPos",
                  fluidPage(
                    fluidRow(
                      fileInput('desemppos', 'Upload - Desempenho para Posicionamento dos itens',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      fluidRow(
                        column(2,
                               DT::dataTableOutput("headDP", width = "150%")
                        ),
                        column(7, offset = 3,
                               verbatimTextOutput("summaryDP")  
                        )
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabSCO",
                  fluidPage(
                    fluidRow(
                      fileInput('SCO', 'Upload - .SCO',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      verbatimTextOutput("tableSCO"),
                      verbatimTextOutput("summarySCO")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabEXP",
                  fluidPage(
                    fluidRow(
                      fileInput('EXP', 'Upload - .EXP',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headEXP", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "leituraTabINAmesFP",
                  fluidPage(
                    fluidRow(
                      fileInput('APnames', 'Upload - INAmes para Full Plot',  accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'), width = "95%")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headAPn", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "aberturaTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("VEbuttom", "Abrir banco")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headVE", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "correcaoTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("Corrbuttom", "Corrigir")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headD2", width = "150%"),
                      DT::dataTableOutput("headall", width = "150%"),
                      plotOutput("plotProp")
                    )
                  )
          ),
          
          tabItem(tabName = "escoreTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("Escbuttom", "Calcular")
                    ),
                    mainPanel(
                      plotOutput("plothist"),
                      plotOutput("plotdens")
                    )
                  )
          ),
          
          tabItem(tabName = "agiTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("AGIbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewAGIit", "DEMO AGI:", 1)),
                      mainPanel(
                        plotOutput("viewAGI")
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "tctTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("TCTbuttom", "Gerar TCT")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headTCT", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "posTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("POSbuttom", "Posicionar Itens")
                    ),
                    mainPanel(
                      DT::dataTableOutput("headPOS", width = "150%")
                    )
                  )
          ),
          
          tabItem(tabName = "cciTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("CCIbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewCCIit", "DEMO CCI:", 1)),
                      mainPanel(
                        plotOutput("viewCCI")
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "difTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("DIFbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewDIFit", "DEMO DIF:", 1)),
                      mainPanel(
                        plotOutput("viewDIF")
                      )
                    )
                  )
          ),
          
          tabItem(tabName = "allTabAction",
                  fluidPage(
                    fluidRow(
                      actionButton("ALLbuttom", "Gerar PDF"),
                      menuSubItem(icon = NULL, numericInput("viewALLit", "DEMO FULL PLOT:", 1)),
                      mainPanel(
                        plotOutput("viewALL")
                      )
                    )
                  )
          )
          
        )
      )
    )
    
    server <- function(input, output) {
      
      output$Intro <- renderText({ "Intro.." })
      output$res <- renderText({
        paste("You've selected:", input$tabs)
      })
      
      ### LEITURA
      observeEvent(input$maxuploadButtom, {
        options(shiny.maxRequestSize = input$maxupload * 1024^2) 
      })
      
      output$headD <- DT::renderDataTable({
        inFile <- input$dados
        if (is.null(inFile))
          return(NULL)
        dados <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        head(dados, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headM <-DT::renderDataTable({
        inFile <- input$mapa
        if (is.null(inFile))
          return(NULL)     
        mapa <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        head(mapa, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headB <- DT::renderDataTable({
        inFile <- input$bib
        if (is.null(inFile))
          return(NULL)     
        bib <<- as.list(read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE))
        head(as.data.frame(bib), input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headP <- DT::renderDataTable({
        inFile <- input$pars
        if (is.null(inFile))
          return(NULL)     
        pars <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        names(pars) = c("Codigo", "a", "b", "c", "a01")[1:ncol(pars)]
        head(pars, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      output$headDP <- DT::renderDataTable({
        inFile <- input$desemppos
        if (is.null(inFile))
          return(NULL)     
        desemppos <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        head(desemppos, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      output$summaryDP <- renderPrint({
        inFile <- input$desemppos
        if (is.null(inFile))
          return(invisible(NULL))     
        desemppos <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)[[1]]
        cat("\n Summary Desempenho: \n")
        summary(desemppos)
      }) 
      
      output$tableSCO <- renderPrint({
        inFile <- input$SCO
        if (is.null(inFile))
          return(invisible(NULL))     
        SCO <<- read.SCO(readLines(inFile$datapath), summary = FALSE)
        cat("Frequencia Grupos: \n")
        table(SCO[[1]])
      })   
      
      output$summarySCO <- renderPrint({
        inFile <- input$SCO
        if (is.null(inFile))
          return(invisible(NULL))
        SCO <<- read.SCO(readLines(inFile$datapath), summary = FALSE)
        cat("\n Summary SCO: \n")
        summary(SCO[[2]])
      })    
      
      output$headEXP <- DT::renderDataTable({
        inFile <- input$EXP
        if (is.null(inFile))
          return(NULL)     
        EXP <<- remakeEXP(readLines(inFile$datapath))
        head(EXP[[1]], input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      output$headAPn <- DT::renderDataTable({
        inFile <- input$APnames
        if (is.null(inFile))
          return(NULL)     
        APnames <<- read.csv2(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote, stringsAsFactors = FALSE)
        names(APnames) = "INAmes"
        head(APnames, input$nhead)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F))
      
      ### ABERTURA
      VE <- eventReactive(input$VEbuttom, {
        if(is.null(input$tipoVE) | input$VEbuttom == 0)
          return(NULL)
        nblocos <- bib %>% unlist %>% unique %>% length
        if(input$tipoVE == "bib"){
          dados <<- vetor.estendido(dados, bib, nblocos, input$b.len)
        }else if(input$tipoVE == "codit"){
          dados <<- vetor.estendido2(dados, bib)
        }
      })
      
      output$headVE <- DT::renderDataTable({
        VE()
        head(dados, input$nheadVE)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F, lengthChange = T, pageLength = 50))
      
      ### CORRECAO
      output$headD2 <- DT::renderDataTable({
        Corr()
        head(dadosCorr[-c(1:3)], 6)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      Corr <- eventReactive(input$Corrbuttom, {
        if(input$Corrbuttom == 0)
          return(NULL)
        dadosCorr <<- corrigir(dados, mapa$Gabarito)
        return(make.headall(dadosCorr))
      })
      
      make.headall <- function(dadosCorr){
        if(input$Corrbuttom == 0)
          return(NULL)
        df <- data.frame(matrix(NA, nrow = 3, ncol = nrow(mapa)))
        df[1, ] <- dados[input$Corrline, -c(1:3)]
        df[2, ] <- mapa$Gabarito    
        df[3, ] <- dadosCorr[input$Corrline, -c(1:3)]      
        aux <- data.frame(matrix(unlist(lapply(strsplit(as.vector(summary(dadosCorr[-c(1:3)])), ":"), FUN = function(x) as.numeric(x[[2]]))), nrow = 7))
        names(aux) <- names(df)
        df <- rbind(df, aux)
        names(df) <- names(dadosCorr[-c(1:3)])
        aux <- data.frame(Info = c("Resposta_Linha", "Gabarito_Item", "Correcao_Linha", "Minimo_Item", "1st Quart._Item", "Mediana_Item", "Media_Item", "3rd Quart._Item", "Maximo_Item", "Missings_Item"))
        df <- cbind(aux, df)
        return(df)
      }
      
      output$headall <- DT::renderDataTable({
        input$Corrline
        make.headall(dadosCorr)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)   
      
      output$plotProp <- renderPlot({
        if(input$Corrbuttom == 0)
          return(invisible(NULL))
        plot(colMeans(dadosCorr[-c(1:3)], na.rm = T), xlab = "Item", ylab = "Proporcao de Acertos", cex = .8, lwd = 4, ylim = c(0, 1))
      })   
      
      ### ESCORE
      Esc <- eventReactive(input$Escbuttom, {
        if(input$Escbuttom == 0)
          return(NULL)
        dados <<- dados[1:(nrow(mapa) + 3)]; dadosCorr <<- dadosCorr[1:(nrow(mapa) + 3)]
        dadosCorr <<- escore(dadosCorr, input$tipoEsc)
        dados[names(dadosCorr)[ncol(dadosCorr)]] <<- dadosCorr[[names(dadosCorr)[ncol(dadosCorr)]]]
      })
      
      output$plothist <- renderPlot({
        Esc()
        hist(dadosCorr[[ncol(dadosCorr)]], xlab = names(dadosCorr)[ncol(dadosCorr)], main = "")
      })
      
      output$plotdens <- renderPlot({
        Esc()
        plot(density(dadosCorr[[ncol(dadosCorr)]]), xlab = names(dadosCorr)[ncol(dadosCorr)], main = "")
      })  
      
      ### AGI
      readplotAGI <- function(){
        if(input$agi.xlim == "NULL"){
          xlim <- NULL
        }else{
          xlim <- as.numeric(unlist(strsplit(input$agi.xlim, ";")))
        }
        if(input$agi.smooth == "NULL"){
          smooth <- NULL
        }else{
          smooth <- as.numeric(input$agi.smooth)
        }
        if(input$agi.files == "NULL"){
          files <- NULL
        }else{
          files <- unlist(strsplit(input$agi.files, ";"))
        }
        if(input$agi.col == "NULL"){
          col.agi <- NULL
        }else{
          col.agi <- unlist(strsplit(input$agi.col, ";"))
        }
        if(input$agi.redGab == "NULL"){
          redGab <- mapa$Gabarito
        }else{
          redGab <- unlist(strsplit(input$agi.redGab, ";"))
        }
        paramsAGI <<- list(
          redGab = redGab,
          main = unlist(strsplit(input$agi.main, ";")),
          files = files,
          xlab = input$agi.xlab,
          Alts = unlist(strsplit(input$agi.Alts, ";")),
          col.agi = col.agi,
          cex = as.numeric(unlist(strsplit(input$agi.cex, ";"))),
          density = input$agi.density %in% c("TRUE", "T", "t", "true", "True"),
          smooth = smooth,
          dir.create = input$agi.dir.create,
          xlim = xlim,
          width = input$agi.width,
          height = input$agi.height
        )
        if(length(paramsAGI$main) < nrow(mapa))
          paramsAGI$main <<- c(paramsAGI$main, paste0("IT Cod: ", mapa$Codigo[(length(paramsAGI$main) + 1):nrow(mapa)]))
      }
      
      output$viewAGI <- renderPlot({
        readplotAGI()
        agi.plot(dados[-c(1:3, ncol(dados))], dados[[ncol(dados)]], redGab = paramsAGI$redGab, main = paramsAGI$main, files = paramsAGI$files, xlab = paramsAGI$xlab, Alts = paramsAGI$Alts, col.agi = paramsAGI$col.agi, cex = paramsAGI$cex, density = paramsAGI$density, smooth = paramsAGI$smooth, dir.create = paramsAGI$dir.create, xlim = paramsAGI$xlim, width = paramsAGI$width, height = paramsAGI$height, shinyDemo = as.numeric(input$viewAGIit))
      })
      
      observeEvent(input$AGIbuttom, {
        readplotAGI()
        agi.plot(dados[-c(1:3, ncol(dados))], dados[[ncol(dados)]], redGab = paramsAGI$redGab, main = paramsAGI$main, files = paramsAGI$files, xlab = paramsAGI$xlab, Alts = paramsAGI$Alts, col.agi = paramsAGI$col.agi, cex = paramsAGI$cex, density = paramsAGI$density, smooth = paramsAGI$smooth, dir.create = paramsAGI$dir.create, xlim = paramsAGI$xlim, width = paramsAGI$width, height = paramsAGI$height)
      })
      
      ### TCT
      TCT <- eventReactive(input$TCTbuttom, {
        if(input$TCTbuttom == 0)
          return(NULL)
        tct.result <<- tct(dados, dadosCorr, mapa, unlist(strsplit(input$tct.Alts, ";")), input$tct.arred, input$tct.sdpop, input$tct.summary)
        if(input$tct.summary){
          write.csv2(tct.result[[2]], "Summary.csv", row.names = F)
          tct.result <<- tct.result[[1]]
        }
        write.csv2(tct.result, "TCT.csv", row.names = F)
      })
      
      output$headTCT <- DT::renderDataTable({
        TCT()
        head(tct.result, input$nheadTCT)
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F) 
      
      ### Posicionamento
      POS <- eventReactive(input$POSbuttom, {
        if(input$POSbuttom == 0)
          return(NULL)
        if(input$pos.method == "model"){
          if(!exists("pars"))
            stop("Dataset pars nao encontrado.")
          names(pars) <- c("Codigo", "a", "b", "c", "a01")
          posicionamento <<- data.frame(Codigo = pars$Codigo, Nivel = pos.model(pars, as.numeric(unlist(strsplit(input$pos.breaks, ";"))), list(PROP = input$pos.PROP, PROP2 = input$pos.PROP2, DIF = input$pos.DIF)), stringsAsFactors = F)
        }else{
          posicionamento <<- data.frame(Codigo = names(dadosCorr[-c(1:3, ncol(dados))]), Nivel = pos.prop(dadosCorr[-c(1:3, ncol(dados))], desemppos, dadosCorr[[3]], as.numeric(unlist(strsplit(input$pos.breaks, ";"))), list(PROP = input$pos.PROP, NRESP = input$pos.NRESP, PROP2 = input$pos.PROP2, DIF = input$pos.DIF)), stringsAsFactors = F)
        }
        posicionamento$Nivel <- as.numeric(posicionamento$Nivel)
        write.csv2(posicionamento, paste0("Posicionamento_", input$pos.method, ".csv"), row.names = F)
      })
      
      output$headPOS <- DT::renderDataTable({
        POS()
        posicionamento
      }, options = list(scrollX = T, bFilter = F, bSort = F, bInfo = F, bPaginate = F), rownames = F)
      
      ### CCI
      readplotCCI <- function(){
        if(input$cci.files == "NULL"){
          files <- NULL
        }else{
          files <- unlist(strsplit(input$cci.files, ";"))
        }
        if(input$cci.SCOg == "NULL"){
          plotSCO <- NULL
        }else{
          plotSCO <- SCO[SCO[[1]] %in% unlist(strsplit(input$cci.SCOg, ";")), 2]
        }
        if(input$cci.breaks == "NULL"){
          breaks <- NULL
        }else{
          breaks <- as.numeric(unlist(strsplit(input$cci.breaks, ";")))
        }
        paramsCCI <<- list(
          main = unlist(strsplit(input$cci.main, ";")),
          SCO = plotSCO,
          breaks = breaks,
          files = files,
          dir.create = input$cci.dir.create,
          width = input$cci.width,
          height = input$cci.height
        )
        if(length(paramsCCI$main) < nrow(mapa))
          paramsCCI$main <<- c(paramsCCI$main, paste0("IT Cod: ", mapa$Codigo[(length(paramsCCI$main) + 1):nrow(mapa)]))
        names(pars) <<- c("Codigo", "a", "b", "c", ifelse(input$cci.info, "a01", "NoInfo"))[1:ncol(pars)]
      }
      
      output$viewCCI <- renderPlot({
        readplotCCI()
        cci.plot(pars, main = paramsCCI$main, SCO = paramsCCI$SCO, breaks = paramsCCI$breaks, files = paramsCCI$files, dir.create = paramsCCI$dir.create, width = paramsCCI$width, height = paramsCCI$height, shinyDemo = as.numeric(input$viewCCIit) )
      })
      
      observeEvent(input$CCIbuttom, {
        readplotCCI()
        cci.plot(pars, main = paramsCCI$main, SCO = paramsCCI$SCO, breaks = paramsCCI$breaks, files = paramsCCI$files, dir.create = paramsCCI$dir.create, width = paramsCCI$width, height = paramsCCI$height)
      })
      
      ### DIF
      readplotDIF <- function(){
        if(input$dif.groups == "NULL"){
          groups <- NULL
        }else{
          groups <- unlist(strsplit(input$dif.groups, ";"))
        }
        if(input$dif.col == "NULL"){
          col <- NULL
        }else{
          col <- unlist(strsplit(input$dif.col, ";"))
        }
        if(input$dif.density == "NULL"){
          density <- NULL
        }else{
          density <- input$dif.density
        }
        if(input$dif.main == "NULL"){
          main <- NULL
        }else{
          main <- input$dif.main
        }
        paramsDIF <<- list(
          newGroup = unlist(strsplit(input$dif.newGroup, ";")),
          crit = input$dif.crit,
          probs = as.numeric(unlist(strsplit(input$dif.probs, ";"))),
          main = main,
          groups = groups,
          col = col,
          density = density,
          dir.create = input$dif.dir.create,
          xlim = as.numeric(unlist(strsplit(input$dif.xlim, ";"))),
          width = input$dif.width,
          height = input$dif.height
        )
        EXPtemp <<- calcDIF(EXP, paramsDIF$newGroup)
        DIF <<- checkDIF(EXPtemp, SCO, newGroup = paramsDIF$newGroup, crit = paramsDIF$crit, probs = paramsDIF$probs)
        if(length(paramsDIF$main) < length(EXPtemp))
          paramsDIF$main <<- c(paramsDIF$main, paste0("IT: ", names(EXPtemp)[(length(paramsDIF$main) + 1):length(EXPtemp)]))
      }
      
      output$viewDIF <- renderPlot({
        readplotDIF()
        AUX <<- EXPtemp[names(EXPtemp) %in% c(as.character(DIF[[1]][, 1]), as.character(DIF[[2]][, 1]))]
        if(as.numeric(input$viewDIFit) <= length(AUX))
          dif.plot(AUX, SCO, main = names(AUX), groups = paramsDIF$groups, probs = paramsDIF$probs, col.dif = paramsDIF$col, density = paramsDIF$density, dir.create = paramsDIF$dir.create, xlim = paramsDIF$xlim, width = paramsDIF$width, height = paramsDIF$height, shinyDemo = as.numeric(input$viewDIFit))
      })
      
      output$ITsDIF <- renderPrint({
        readplotDIF()
        DIF
      })  
      
      observeEvent(input$DIFbuttom, {
        readplotDIF()
        dif.plot(EXPtemp, SCO, main = paramsDIF$main, groups = paramsDIF$groups, probs = paramsDIF$probs, col.dif = paramsDIF$col, density = paramsDIF$density, dir.create = paramsDIF$dir.create, xlim = paramsDIF$xlim, width = paramsDIF$width, height = paramsDIF$height)
        write.csv2(DIF$DIF, "DIF.csv", row.names = F)
        write.csv2(DIF$Ajuste, "Ajuste.csv", row.names = F)
      })
      
      ### Full Plot
      readplotALL <- function(){
        if(input$all.main == "NULL"){
          main <- APnames[[1]]
        }else{
          main <- unlist(substr(input$all.main, ";"))
        }
        if(input$all.xlim.agi == "NULL"){
          xlim.agi <- NULL
        }else{
          xlim.agi <- as.numeric(unlist(strsplit(input$all.xlim.agi, ";")))
        }
        if(input$all.smooth == "NULL"){
          smooth <- NULL
        }else{
          smooth <- as.numeric(input$all.smooth)
        }
        if(input$all.files == "NULL"){
          files <- APnames[[1]]
        }else{
          files <- unlist(strsplit(input$all.files, ";"))
        }
        if(input$all.col.agi == "NULL"){
          col.agi <- NULL
        }else{
          col.agi <- unlist(strsplit(input$all.col.agi, ";"))
        }
        if(input$all.redGab == "NULL"){
          redGab <- mapa$Gabarito
        }else{
          redGab <- unlist(strsplit(input$all.redGab, ";"))
        }    
        if(input$all.breaks.cci == "NULL"){
          breaks.cci <- NULL
        }else{
          breaks.cci <- as.numeric(unlist(strsplit(input$all.breaks.cci, ";")))
        }    
        if(input$all.groups == "NULL"){
          groups <- NULL
        }else{
          groups <- unlist(strsplit(input$all.groups, ";"))
        }
        if(input$all.col.dif == "NULL"){
          col.dif <- NULL
        }else{
          col.dif <- unlist(strsplit(input$all.col.dif, ";"))
        }
        
        paramsALL <<- list(
          xlab = unlist(strsplit(input$all.xlab, ";")),
          ylab = unlist(strsplit(input$all.ylab, ";")),
          main = main,
          files = files,
          redGab = redGab,
          xlim.agi = xlim.agi,
          smooth = smooth,
          Alts = unlist(strsplit(input$all.Alts, ";")),
          col.agi = col.agi,
          cex = as.numeric(unlist(strsplit(input$all.cex, ";"))),
          breaks.cci = breaks.cci,
          groups = groups,
          probs = as.numeric(unlist(strsplit(input$all.probs, ";"))),
          col.dif = col.dif,
          xlim.dif = as.numeric(unlist(strsplit(input$all.xlim.dif, ";"))),
          dir.create = input$all.dir.create,
          width = input$all.width,
          height = input$all.height
        )
        if(length(paramsALL$main) < length(APnames[[1]]))
          paramsALL$main <<- c(paramsALL$main, paste0("IT: ", APnames[[1]][(length(paramsALL$main) + 1):length(APnames[[1]])]))
        names(pars) <<- c("Codigo", "a", "b", "c", ifelse(input$all.info.cci, "a01", "NoInfo"))[1:ncol(pars)]
      }
      
      output$viewALL <- renderPlot({
        readplotALL()
        all.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr[[ncol(dadosCorr)]], pars = pars, EXP = EXP[APnames[[1]]], SCO = SCO, xlab = paramsALL$xlab, ylab = paramsALL$ylab,
                 main = paramsALL$main, files = paramsALL$files, redGab = paramsALL$redGab, xlim.agi = paramsALL$xlim.agi, smooth = paramsALL$smooth,
                 Alts = paramsALL$Alts, col.agi = paramsALL$col.agi, cex = paramsALL$cex, breaks = paramsALL$breaks.cci, groups = paramsALL$groups,
                 probs = paramsALL$probs, col.dif = paramsALL$col.dif, xlim.dif = paramsALL$xlim.dif, dir.create = paramsALL$dir.create, width = paramsALL$width, height = paramsALL$height, shinyDemo = as.numeric(input$viewALLit))
      })
      
      observeEvent(input$ALLbuttom, {
        readplotALL()
        all.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr[[ncol(dadosCorr)]], pars = pars, EXP = EXP[APnames[[1]]], SCO = SCO, xlab = paramsALL$xlab, ylab = paramsALL$ylab,
                 main = paramsALL$main, files = paramsALL$files, redGab = paramsALL$redGab, xlim.agi = paramsALL$xlim.agi, smooth = paramsALL$smooth,
                 Alts = paramsALL$Alts, col.agi = paramsALL$col.agi, cex = paramsALL$cex, breaks = paramsALL$breaks.cci, groups = paramsALL$groups,
                 probs = paramsALL$probs, col.dif = paramsALL$col.dif, xlim.dif = paramsALL$xlim.dif, dir.create = paramsALL$dir.create, width = paramsALL$width, height = paramsALL$height)
      })
      
    }
    
    shinyApp(ui, server)
  }

