% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pos.prop.R
\name{pos.prop}
\alias{pos.prop}
\title{Calcula o posicionamento empirico}
\usage{
pos.prop(dadosCorr, desemp, PESO = NULL, breaks = c(-5, 5, 0.1),
  crit = list(PROP = 0.65, NRESP = 50, PROP2 = 1, DIF = 0))
}
\arguments{
\item{dadosCorr}{Banco de dados corrigido a ser utilizado. Este deve ser composto com os seguintes campos: (IT1, ..., ITm).}

\item{desemp}{Medida de desempenho a ser utilizada.}

\item{PESO}{Vetor com os pesos referentes aos casos de 'dadosCorr'.}

\item{breaks}{Limite inferior e superior dos pontos de posicionamento, acompanhados pelo step da sequencia.}

\item{crit}{Lista com os criterios para posicionamento.
\itemize{
 \item{PROP}{: Proporcao minima de respostas corretas no nivel}.
 \item{NRESP}{: Numero minimo de respostas no nivel}.
 \item{PROP2}{: Proporcao maxima de respostas corretas no nivel anterior}.
 \item{DIF}{: Diferenca minima entre PROP e PROP2}.
}}
}
\value{
Vetor com o posicionamento (se possivel) de cada item.
}
\description{
Calcula o posicionamento empirico dos itens atraves das respostas observadas.
}
\details{
Etapa Anterior: 'escore'.
}
\examples{
bib = list(CAD1 = c(1,2), CAD2 = c(2,3), CAD3 = c(3,1))

nblocos = 3

b.len = 2

resp = data.frame(matrix(sample(c("A", "B", "C"), 40, replace = T), ncol = 4), stringsAsFactors = F); names(resp) = paste0("IT", 1:4)

dados = cbind(data.frame(ID = 1:10, CAD = paste0("CAD", sample(3, 10, replace = T)), PESO = 1), resp)

dados = vetor.extendido(dados, bib, nblocos, b.len)

dadosCorr = corrigir(dados, c("A", "B", "A", "C", "A", "B"))

dadosCorr = escore(dadosCorr)

POS = pos.prop(dadosCorr[paste0("IT", 1:6)], 100 / 6 * dadosCorr$Escore, breaks = c(0, 100, 10), crit = list(PROP = 0.65, NRESP = 20))

}
\seealso{
pos.model()
}
\author{
Brunno Bittencourt
}
