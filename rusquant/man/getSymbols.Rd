\name{getSymbols}
\alias{getSymbols}
\title{Load and manage market data}
\description{
  Function to load and manage data.
  Current src methods available are: yahoo, Finam, mfd, rogov, oanda, google.
  Current period methods available are: sec, hour, day, week, month, year.
}
\usage{
getSymbols(Symbols = NULL,  from='2007-01-01', to = Sys.Date(), src = "yahoo", period = "day")
}
\arguments{
  \item{Symbols}{a character vector specifying the names of each symbol (ticker) to be loaded.}
  \item{from}{start date of analyzing period.}
  \item{to}{end date of analyzing period.}
  \item{src}{character string specifying sourcing method (only yahoo, Finam, mfd, rogov, oanda, google).}
  \item{period}{calendar periods for Finam ("tick","secs", "seconds", "mins", "hours", "days", "weeks", "months", "quarters", "years") .}	
}
\examples{
getSymbols('AFLT',src='Finam')
}
\keyword{getSymbols}