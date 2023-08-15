#' @title Download alpha strategy data from Rusquant
#'
#' @description This function retrieves alpha data for the specified alpha,symbols from Rusquant API.
#' The data can be returned in either xts or data.frame format.
#'
#' @param Symbols a character vector of symbols to be downloaded.
#' @param env the environment where the data should be stored. Default is the global environment.
#' @param field the name(s) of alpha strategy to retrieve from the API. Default is NULL.
#' @param from the start date of the data to be retrieved. Default is '2007-01-01'.
#' @param to the end date of the data to be retrieved. Default is Sys.Date().
#' @param period a character value indicating the periodicity of the data. Default is 'day'.
#' @param market a character value indicating the market where the symbols are traded. Default is NULL.
#' @param api.key a character value indicating the API key to be used for the request. Default is NULL.
#' @param verbose a logical value indicating whether to print informative messages. Default is FALSE.
#' @param auto.assign a logical value indicating whether to automatically assign the downloaded data to an object with the symbol name. Default is FALSE.
#' @param ... Additional arguments to be passed to functions.
#' @return returns an data.table object containing the requested data with alpha strategy data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' \donttest{
#' rusquant_key = 'get free key from rusquant.ru'
#' getSymbols.Rusquant('SBER',field = 'A1_L_P1',api.key = rusquant_key)
#' getSymbols('SBER',src='Rusquant',field = 'A1_L_P1',api.key = rusquant_key)
#' }
#' @export

"getSymbols.Rusquant" <-
  function(Symbols,env = globalenv(),
           field = NULL,
           from='2007-01-01',
           to=Sys.Date(),
           period='day',
           market=NULL,
           api.key=NULL,
           verbose=FALSE,
           auto.assign=FALSE,
           ...)
{
  Symbol.name = Symbols[[1]]
  rusquant.url <- "https://api.rusquant.io/alpha"
  rusquant.params = list('symbol' = Symbol.name,
                         'alpha'=field,
                         'market'=market,
                         'from' = from,
                         'to' = to,
                         'token' = api.key)

  response <- GET(rusquant.url,query = rusquant.params)
  data <- data.table(fromJSON(content(response, "text",encoding = 'UTF-8')))

  Symbol.name <-toupper(gsub('\\^','',Symbol.name))
  if(auto.assign) assign(Symbol.name,data,env)
  if(auto.assign) return(Symbols)
return(data)
}
