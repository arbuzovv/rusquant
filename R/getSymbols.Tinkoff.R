#' @title Get earnings data from investing.com
#'
#' @description Retrieve financial data from Tinkoff API.
#'
#' @param Symbols A character vector with the names of the symbols to be retrieved.
#' @param from A character string representing the start date for the financial data, in the format "YYYY-MM-DD". By default, it is "2007-01-01".
#' @param to A character string representing the end date for the financial data, in the format "YYYY-MM-DD". By default, it is the current system date.
#' @param adjust A logical value indicating whether to adjust prices for dividends and stock splits. By default, it is FALSE.
#' @param api.key A character string containing the API key for accessing Tinkoff API.
#' @param period A character string representing the interval of time between two candles. By default it is "day".
#' @param market A character string representing the market to which the symbol belongs. By default, it is NULL.
#' @param verbose A logical value indicating whether to print verbose output. By default, it is FALSE.
#' @param auto.assign A logical value indicating whether to automatically assign the downloaded data to an object with the same name as the symbol. By default, it is FALSE.
#' @param ... Additional arguments.
#' @return A data table with the financial data for the specified symbol(s).
#'
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Tinkoff("BBG004730N88",from=Sys.Date()-5, ap.key = "your_api_key",verbose=TRUE)
#' @export

"getSymbols.Tinkoff" <-
  function(Symbols,
           from='2007-01-01',
           to=Sys.Date(),
           adjust=FALSE,
           api.key=NULL,
           period='day',
           market=NULL,
           verbose=FALSE,
           auto.assign=FALSE,
           ...)
  {

  periods = c('1min' = 'CANDLE_INTERVAL_1_MIN', '5min' = 'CANDLE_INTERVAL_5_MIN', '15min' = 'CANDLE_INTERVAL_15_MIN', 'hour' = 'CANDLE_INTERVAL_HOUR','day' =  'CANDLE_INTERVAL_DAY', '2min' = 'CANDLE_INTERVAL_2_MIN', '3min' =  'CANDLE_INTERVAL_3_MIN','10min' =  'CANDLE_INTERVAL_10_MIN','30min' =  'CANDLE_INTERVAL_30_MIN','2hour' =  'CANDLE_INTERVAL_2_HOUR', '4hour' =  'CANDLE_INTERVAL_4_HOUR','week' =   'CANDLE_INTERVAL_WEEK','month' =   'CANDLE_INTERVAL_MONTH')
  tcs.interval = periods[period]

  url <- "https://invest-public-api.tinkoff.ru/rest/"
  endpoint <- "tinkoff.public.invest.api.contract.v1.MarketDataService/GetCandles"
  full_url <- paste0(url, endpoint)
  body <- list(figi=Symbols,
               from= paste0(from,"T16:08:09.556Z"),
               to=paste0(to,"T16:08:09.556Z"),
               interval= tcs.interval,
               instrumentId= "string")
  headers <- c("Authorization" = paste("Bearer", api.key))
  response <- POST(full_url, body = body, encode = "json", add_headers(headers))
  if(response$status_code==200)
  {
    json_response <- content(response, "text", encoding = "UTF-8")
    parsed_response <- fromJSON(json_response)$candles
    data_result = data.table(
      date = as.POSIXct(strptime(parsed_response$time,'%Y-%m-%dT%H:%M:%S',tz='GMT')),
      open = as.numeric(parsed_response$open$units)+as.numeric(parsed_response$open$nano)/1000000000,
      high = as.numeric(parsed_response$high$units)+as.numeric(parsed_response$high$nano)/1000000000,
      low = as.numeric(parsed_response$low$units)+as.numeric(parsed_response$low$nano)/1000000000,
      close = as.numeric(parsed_response$close$units)+as.numeric(parsed_response$close$nano)/1000000000,
      volume = as.numeric(parsed_response$volume)
    )
    return(data_result)
  }
  if(response$status_code!=200)
    if(verbose) return(content(response, as = "parsed"))
}


