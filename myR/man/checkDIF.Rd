% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkDIF.R
\name{checkDIF}
\alias{checkDIF}
\title{Checa os itens com DIF e mal ajuste}
\usage{
checkDIF(EXP, SCO, newGroup, crit = 0.15, probs = c(0.05, 0.95))
}
\arguments{
\item{EXP}{Objeto resultante da funcao calcDIF.}

\item{SCO}{DF contendo os seguintes campos: (Grupo, SCO) com as proficiencias dos individuos presentes na analise.}

\item{newGroup}{Vetor com os grupos de interesse (so serao plotados os itens com pelo menos um grupo de interesse).}

\item{crit}{Diferenca minima para caracterizar um comportamento diferencial.}

\item{probs}{Quantiles utilizados para o calculo dos intervalos de interesse.}
}
\value{
Lista com dois DFs, apresentando os itens que apresentaram comportamento diferencial e mal ajuste.
}
\description{
Dado o resultado da calcDIF, esta funcao checa se ha, e se houver, quais itens possuem um DIF entre grupos ou com o modelo maior que o criterio especificado.
}
\details{
Apenas serao consideradas as proporcoes entre os quantis passados no argumento SCO.

Etapa Anterior: 'calcDIF' 'read.SCO'.

Etapa Posterior: 'dif.plot'.
}
\examples{
EXP = remakeEXP(readLines("EXPtest.EXP"))

newGroup = c(6, 7)

EXP = calcDIF(EXP, newGroup)

SCO = read.SCO(readLines("SCOtest.SCO"))

SCO = lapply(split(SCO$SCO, SCO$Grupo), quantile, probs = c(.05, .95))

DIF = checkDIF(EXP, SCO, newGroup)

}
\author{
Brunno Bittencourt
}
