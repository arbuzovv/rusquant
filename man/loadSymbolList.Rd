\name{loadSymbolList}
\alias{loadSymbolList}
\title{Load all available symbols for current data source}
\description{
  Function to load available symbols.
  Current src methods available are: poloniex,kraken,binance,bitfinex,bitstamp,bttrex,cex,gate,gatecoin,gdax,gemini,hitbtc,liqui,lykke,xbtce
}
\usage{
loadSymbolList(src = "poloniex", verbose=FALSE, auto.assign=FALSE)
}
\arguments{
  \item{src}{character string specifying sourcing method (kraken, poloniex, etc).}
 }
\examples{
loadSymbolList()
}
\keyword{getSymbols}