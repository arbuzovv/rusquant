#' @title Download Alor data
#'
#' @description Download historical market data from Alor for a given symbol and time range.
#'
#' @param Symbols a character vector of Alor symbols to download data for.
#' @param env environment where to create the downloaded data object.
#' @param from a character string indicating the start date of the data to download, in YYYY-MM-DD format.
#' @param to a character string indicating the end date of the data to download, in YYYY-MM-DD format.
#' @param adjust a logical indicating whether to adjust the data for stock splits or not.
#' @param api.key an Alor API key.
#' @param period a character string indicating the frequency of the data to download. Possible values are '1min', '5min', 'hour', 'day', 'week', and 'month'.
#' @param verbose a logical indicating whether to print the response details or not.
#' @param board a character string indicating the Alor exchange board to use. Possible values are 'MOEX' and 'SPB'.
#' @param auto.assign a logical indicating whether to automatically assign the downloaded data to the global environment.
#' @param ... additional arguments passed to getSymbols.Alor
#' @return returns an data.table object containing financial data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Alor('SBER',from = '2023-04-01',to='2023-05-04',period = '1min')
#' getSymbols('SBER',src='Alor')
#' @export

getSymbols.Alor <- function(Symbols,
                            env = globalenv(),
                            from='2007-01-01',
                            to=Sys.Date(),
                            adjust=FALSE,
                            api.key=NULL,
                            period='day',
                            verbose=TRUE,
                            board = 'MOEX',
                            auto.assign=FALSE,
                            ...)
{
  headers = ''
  if(!is.null(api.key))
  {
    jwt_token = POST(paste0('https://oauth.alor.ru/refresh?token=',api.key))
    jwt_token <- fromJSON(content(jwt_token, "text"))$AccessToken
    headers <- c("Authorization" = paste0("Bearer ", jwt_token))
  }
  for(i in 1:length(Symbols))
  {
  Symbols.name = Symbols[i]
  alor.from <- format(as.numeric(as.POSIXct(from)),scientific = F) #convert to UNIX timestamp
  alor.to <- format(as.numeric(as.POSIXct(to)),scientific = F) #convert to UNIX timestamp
  periods = c('1min'=60,'5min'=300,'hour'=3600,'day'='D','week'='W','month'='M')
  alor.period = periods[period]
  alor.downloadUrl <- 'https://apidev.alor.ru/md/v2/history'
  alor.params = list('symbol' = Symbols.name,
                     'exchange' = board,
                     'tf' = as.character(alor.period),
                     'from'=alor.from,
                     'to'=alor.to,
                     'format'='Simple')
  response <- GET(alor.downloadUrl, encode = "json", add_headers(headers),query = alor.params)

  if(response$status_code==200)
  {
    json_response <- content(response, "text", encoding = "UTF-8")
    data_result <- fromJSON(json_response)
    data_result = data.table(data_result$history)
    data_result$time = as.POSIXct(as.numeric(data_result$time), origin="1970-01-01")
    names(data_result)[1] = 'DateTime'
    data_result$Date = as.Date(data_result$DateTime)
  }
  if(response$status_code!=200)
    if(verbose) return(content(response, as = "parsed"))

  if(auto.assign){
    assign(Symbols[[i]], data_result, env)
  }
  }
  if(auto.assign){
    return(Symbols)
  }
  return(data_result)
}


