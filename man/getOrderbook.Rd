\name{getOrderbook}
\alias{getOrderbook}
\title{Load current snapshot of order book}
\description{
  Function to load order book.  Current src methods available are: poloniex,kraken,binance,bttrex,cex,gate,gatecoin,gdax,gemini,hitbtc,liqui,lykke,xbtce
}
\usage{
getOrderbook(Symbols = NULL, src='poloniex', depth = 10)
}
\arguments{
  \item{Symbols}{a character vector specifying the names of each symbol (ticker) to be loaded.}
  \item{src}{character string specifying sourcing method (kraken, poloniex, etc).}
  \item{depth}{depth of the order book.}
 }
\keyword{getOrderbook}