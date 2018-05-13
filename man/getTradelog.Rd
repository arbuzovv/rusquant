\name{getTradelog}
\alias{getTradelog}
\title{Load trade data}
\description{
  Function to load and manage trade data. Current src methods available are: poloniex,kraken,binance,bttrex,cex,gate,gatecoin,gdax,gemini,hitbtc,liqui,lykke.
}
\usage{
getTradelog(Symbols = NULL,  from=Sys.Date(), to = Sys.Date())
}
\arguments{
  \item{Symbols}{a character vector specifying the names of each symbol (ticker) to be loaded.}
  \item{from}{start date of analyzing period.}
  \item{to}{end date of analyzing period.}
 }
\examples{
exchange <- 'poloniex'
list_s <- loadSymbolList(src = exchange)
Symbol <- list_s$Symbol[41]
getTradelog(Symbols = Symbol,src = exchange,depth = 500)
}
\keyword{getTradelog}