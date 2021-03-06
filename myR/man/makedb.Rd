% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makedb.R
\name{makedb}
\alias{makedb}
\title{Formatar o banco de dados}
\usage{
makedb(dados)
}
\arguments{
\item{dados}{Banco de dados com as respostas.}
}
\value{
DF com a seguinte composicao: (ID, CAD, PESO, IT1, ..., ITn, ..., ITm).
}
\description{
Cria as colunas (ID, CAD, PESO) em um banco para montar o format requerido nas demais funcoes.
}
\examples{
resp = data.frame(matrix(sample(c("A", "B", "C"), 40, replace = T), ncol = 4), stringsAsFactors = F); names(resp) = paste0("IT", 1:4)

resp = makedb(resp)

}
\author{
Brunno Bittencourt
}
