#' @title MarketWatch Alor data
#'
#' @description This function retrieves historical financial data for a given symbol from the MarketWatch website and returns it as a data frame or assigns it to an R object.
#'
#' @param Symbols A character vector specifying the name of the financial instrument(s) to retrieve.
#' @param env An environment where the data should be loaded.
#' @param from A character string specifying the starting date for the historical data in the format "YYYY-MM-DD".
#' @param to A character string specifying the ending date for the historical data in the format "YYYY-MM-DD".
#' @param adjust A logical value indicating whether to adjust the prices for splits and dividends. The default value is FALSE.
#' @param period A character string specifying the period for which to retrieve the data. The default value is "day".
#' @param market A character string specifying the market where the financial instrument is listed.
#' @param countrycode A character string specifying the country code of the market where the financial instrument is listed.
#' @param verbose A logical value indicating whether to print additional information while running the function. The default value is FALSE.
#' @param auto.assign A logical value indicating whether to assign the data to an R object with the same name as the symbol. The default value is FALSE.
#' @param ... Additional arguments.
#'
#' @return A data frame or an object of class "xts" containing the historical financial data for the given symbol.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.MarketWatch(Symbols = 'liborusd3m',market = 'interestrate',countrycode = 'mr')
#' getSymbols.MarketWatch(Symbols = 'tmubmusd03m',market = 'bond',countrycode = 'bx')
#' @export

"getSymbols.MarketWatch" <-
  function(Symbols,env=globalenv(),
           from='2007-01-01',
           to=Sys.Date(),
           adjust=FALSE,
           period='day',
           market=NULL,
           countrycode=NULL,
           verbose=FALSE,
           auto.assign=FALSE,
           ...)
  {
    Symbol.name = Symbols[1]
    endpoint <- "https://www.marketwatch.com/investing/"
    mw.from = format(as.Date(from),'%m/%d/%Y')
    mw.to = format(as.Date(to),'%m/%d/%Y')
    full_url <- paste0(endpoint,market,'/' ,Symbol.name,'/downloaddatapartial?startdate=',mw.from,'%2000:00:00&enddate=',mw.to,'%2023:59:59&frequency=p1d&csvdownload=true&countrycode=',countrycode)
    if(verbose==T) print(full_url)
    headers = add_headers('Accept' = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
                          'Accept-Language' = 'en-US,en;q=0.9,fr;q=0.8,ja;q=0.7,es;q=0.6',
                          'Accept-Encoding' = 'gzip, deflate, br',
                          'Connection' = 'keep-alive',
                          'Host' = 'www.marketwatch.com',
                          'Sec-Fetch-Dest' = 'document',
                          'Sec-Fetch-Mode' = 'navigate',
                          'Sec-Fetch-Site' = 'none',
                          'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4.1 Safari/605.1.15')
    r <- GET(full_url,headers)
    df = data.table(content(r))
    Symbol.name <-toupper(gsub('\\^','',Symbol.name))
    if(auto.assign)
      assign(Symbol.name,df,env)

    if(auto.assign) return(Symbol.name)
    return(df)
}
