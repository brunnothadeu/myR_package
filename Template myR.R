#======================================================================================
#Pacotes
library(myR)
loadpkg()


#======================================================================================
#Leituras e manipulações inicias
Alts <- c("A", "B", "C", "D", "E")
pars <- data.frame(a = rlnorm(45, .2, .2), d = rnorm(45, 0, 3), g = .05 + runif(45) / 5)
dados <- data.frame(simdata(a = pars$a, d = pars$d, guess = pars$g, 10000, itemtype = "3PL"))
mapa <- data.frame(Codigo = 8001:8045, Gabarito = sample(Alts, 45, replace = T), Extras = paste0("H", sample(5, 45, replace = T)), stringsAsFactors = F)

#Revertendo a correção
for(i in seq_along(dados)){
  dados[dados[[i]] == 1, i] <- mapa$Gabarito[i]
  dados[dados[[i]] == 0, i] <- sample(Alts[which(!Alts %in% mapa$Gabarito[i])], sum(dados[[i]] == 0), replace = T)
}

#Fechando o banco em cadernos
cad <- sample(1:3, nrow(dados), replace = T)
dados <- data.frame(rbind(dados[cad == 1, 1:30] %>% unlist %>% matrix(ncol = 30),
                          dados[cad == 2, 16:45] %>% unlist %>% matrix(ncol = 30),
                          dados[cad == 3, c(31:45, 1:15)] %>% unlist %>% matrix(ncol = 30))) #, stringsAsFactors = F
dados <- makedb(dados)
dados$CAD <- paste0("CAD", c(rep(1, sum(cad == 1)), rep(2, sum(cad == 2)), rep(3, sum(cad == 3))))


#======================================================================================
#Configurações do teste
bib <- list(CAD1 = c(1,2),
            CAD2 = c(2,3),
            CAD3 = c(3,1))
configCADS <- list(CAD1 = 8001:8030, CAD2 = 8016:8045, CAD3 = c(8031:8045, 8001:8015))


#======================================================================================
#Base
dadosBIB <- vetor.estendido(dados, bib)          #via BI(B)
dadosCOD <- vetor.estendido2(dados, configCADS)  #via Código dos Itens
mean(dadosBIB == dadosCOD, na.rm = T)
dados <- dadosBIB

dadosCorr <- corrigir(dados, mapa$Gabarito)
dadosCorr <- escore(dadosCorr, method = "Normit")  #Escore/Normit
dados$Normit <- dadosCorr$Normit


#======================================================================================
#Análises Clássicas
agi.plot(dados[-c(1:3, ncol(dados))], desemp = dados$Normit, redGab = mapa$Gabarito, main = paste0("IT ", 1:45, " - Cod ", mapa$Codigo, " - Gab ", mapa$Gabarito), files = paste0("AGI ", mapa$Codigo), xlab = "Normit", xlim = c(min(dados$Normit), max(dados$Normit)), smooth = 3, density = F)
TCT <- tct(dados, dadosCorr, mapa, itemResto = "normit", sdpop = TRUE, arred = 3)
TCT$Prop.Nivel <- pos.prop(dadosCorr[-c(1:3, ncol(dadosCorr))], dadosCorr$Normit, breaks = c(-4, 4, .01), crit = list(PROP = 0.65, NRESP = 10))


#======================================================================================
#Análises via TRI
pars <- data.frame(Codigo = mapa$Codigo, a = pars$a, b = -pars$d / pars$a, c = pars$g, a01 = pars$a) #a01 <= Info Curve
TCT$Model.Nivel <- pos.model(pars, breaks = c(-5, 5, 0.1), crit = list(PROP = 0.65, NRESP = 10))

newGroup <- c(6, 7) #Grupos de Interesse
SCO <- read.SCO(readLines("SCOtest.SCO"))
cci.plot(pars, paste0("Cod ", pars$Codigo, "\n a ", round(pars$a, 2), " - b ", round(pars$b, 2), " - c ", round(pars$c, 2)), SCO = SCO$SCO[SCO$Grupo %in% newGroup])

EXP <- remakeEXP(readLines("EXPtest.EXP"))
EXP <- calcDIF(EXP, newGroup)
DIF <- checkDIF(EXP, SCO, newGroup)
DIF
dif.plot(EXP, SCO, main = paste0("IT ", names(EXP)), density = NULL)
full.plot(dados[-c(1:3, ncol(dados))], desemp = dadosCorr$Normit, redGab = mapa$Gabarito, xlim.agi = c(min(dados$Normit), max(dados$Normit)), xlab = c("Normit", "Proficiência", "Proficiência"), smooth = 3, pars = pars, SCO = SCO, EXP = EXP[1:nrow(pars)])

myR.app()