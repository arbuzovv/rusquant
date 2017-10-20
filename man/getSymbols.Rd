\name{getSymbols}
\alias{getSymbols}
\title{Load and manage OHLCV market data}
\description{
  Function to load and manage data.
  Current src methods available are: google, yahoo, Finam, mfd, Alor, Poloniex, oanda.
}
\usage{
getSymbols(Symbols = NULL,  from='2007-01-01', to = Sys.Date(), src = "yahoo", period = "day")
}
\arguments{
  \item{Symbols}{a character vector specifying the names of each symbol (ticker) to be loaded.}
  \item{from}{start date of analyzing period.}
  \item{to}{end date of analyzing period.}
  \item{src}{character string specifying sourcing method (only yahoo, Finam, mfd, rogov, oanda, google).}
  \item{period}{Calendar periods for Finam("tick","1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month"). Calendar periods for mfd("tick","1min", "5min", "10min", "15min", "30min","hour","day","week","month").  Calendar periods for Alor("tick","1min", "5min", "10min", "15min", "20min", "30min", "hour", "day").   Calendar periods for Poloniex("5min","15min", "30min", "2hours", "4hours", "day"). }	
}
\examples{
getSymbols('AFLT',src='Finam')
}
\keyword{getSymbols}