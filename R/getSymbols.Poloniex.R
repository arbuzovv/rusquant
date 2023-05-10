#' @title Get financial data from Poloniex
#'
#' @description This function retrieves financial data from Poloniex.
#'
#' @param Symbols A character vector with the ticker symbols to retrieve data for.
#' @param env The environment where to create the variables. Default is the current environment.
#' @param return.class The class to return. Default is xts.
#' @param index.class The class for the index column. Default is Date.
#' @param from Start date of the data. Default is '2007-01-01'.
#' @param to End date of the data. Default is Sys.Date().
#' @param adjust Logical. Adjust the prices. Default is FALSE.
#' @param period The period for the candle data. Default is 'day'.
#' @param verbose Logical. Print progress messages. Default is TRUE.
#' @param auto.assign Logical. Assign data to the environment. Default is FALSE.
#' @param ... Additional arguments to be passed to functions.
#'
#' @return A list of xts objects if \code{auto.assign} is TRUE, otherwise a single xts object.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Poloniex('BTC_USDT')
#' #getSymbols('BTC_USDT',src='Poloniex')
#'
#' @export

"getSymbols.Poloniex" <- function
(Symbols,env,return.class='xts',index.class='Date',
 from='2007-01-01',
 to=Sys.Date(),
 adjust=FALSE,
 period='day',
 verbose=TRUE,
 auto.assign=FALSE,
 ...)
{
  Polo.period <- 0
  Polo.max_candles <- 1000
  Polo.from <- format(1000*as.numeric(as.POSIXct(from)),scientific = F) #convert to UNIX timestamp
  Polo.to <- format(1000*as.numeric(as.POSIXct(to)),scientific = F) #convert to UNIX timestamp

  Polo.downloadUrl <- 'https://api.poloniex.com/markets/'

  switch(period, #select candle period according to data frequency
         '1min' = {
           Polo.period <- 'MINUTE_1'
         },
         '5min' = {
           Polo.period <- 'MINUTE_5'
         },
         '15min' = {
           Polo.period <- 'MINUTE_15'
         },
         '30min' = {
           Polo.period <- 'MINUTE_30'
         },
         '2hours' = {
           Polo.period <- 'HOUR_2'
         },
         '4hours' = {
           Polo.period <- 'HOUR_4'
         },
         'day' = {
           Polo.period <- 'DAY_1'
         }
  )

  #example API usage
  # https://api.poloniex.com/markets/BTS_BTC/candles?interval=DAY_1&limit=500&startTime=1468711470000&endTime=1682985600000


  for(i in 1:length(Symbols)) {
    Polo.url <- paste(Polo.downloadUrl, Symbols[[i]],
                      "/candles?interval=", Polo.period,
                      "&limit=500&startTime=", Polo.from,
                      "&endTime=", Polo.to, sep="")
    rawdata = data.table(jsonlite::fromJSON(Polo.url, simplifyVector = TRUE))

    rawdata$V13 = as.POSIXct(as.numeric(rawdata$V13)/1000, origin="1970-01-01")
    rawdata$V14 = as.POSIXct(as.numeric(rawdata$V14)/1000, origin="1970-01-01")
    ticker_header <- c('low'	,'high',	'open',	'close',	'amount',	'quantity',	'buyTakerAmount',	'buyTakerQuantity',	'tradeCount',	'ts',	'weightedAverage',	'interval',	'startTime'	,'closeTime')
    names(rawdata) <- ticker_header

    Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))

    if(auto.assign){
      assign(Symbols[[i]], rawdata, env)
    }
  }

  if(auto.assign){
    return(Symbols)
  }

  return(rawdata)
}
