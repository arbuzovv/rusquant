#' @title Get financial data from MOEX Exchange
#'
#' @description Retrieves historical data of a stock from Moscow Exchange (MOEX) using its API.
#'
#' @param Symbols character vector of ticker symbols to retrieve data for
#' @param env environment where data is stored
#' @param from character string specifying the start date (default: '2007-01-01')
#' @param to character string specifying the end date (default: Sys.Date())
#' @param adjust logical flag indicating whether to adjust prices for dividends and splits (default: FALSE)
#' @param period character string specifying the interval of the data ('day', 'hour', '10min', or '1min'; default: 'day')
#' @param market character string specifying the market of the data (default: NULL)
#' @param verbose logical flag indicating whether to print progress messages (default: FALSE)
#' @param auto.assign logical flag indicating whether to automatically assign the resulting data to objects with the same names as Symbols (default: FALSE)
#' @param ... Additional arguments.
#' @return xts object with historical data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#'
#' @examples
#' data <- getSymbols.Moex('GAZP', from='2022-01-01', to='2022-01-10', period='day', auto.assign=FALSE)
#' @export



"getSymbols.Moex" <-
  function(Symbols,env = globalenv(),
             from='2007-01-01',
             to=Sys.Date(),
             adjust=FALSE,
             period='day',
             market=NULL,
             verbose=FALSE,
             auto.assign=FALSE,
             ...)
  {
  old <- options()
  on.exit(options(old))
  options(timeout=7)
  V2 <- V14 <- V15 <- begin <- high <- low <- volume <- NULL
  for(Symbol.name in Symbols)
  {
  periods = c('day'=24, 'hour'=60, '10min'=10, '1min'=1)
  interval = periods[period]

  res = data.table()
  check_ticker_url <- paste0('https://iss.moex.com/iss/securities.json?q=',Symbol.name)
  tryCatch(
    {
      res<-jsonlite::fromJSON(txt=check_ticker_url)
      res<-data.table::data.table(res$securities$data)[V2==Symbol.name]
    },
           #if an error occurs, tell me the error
           error=function(e) {
             message('Server of MOEX not response - try later')
             return(NULL)
             #print(e)
           },
            #if a warning occurs, tell me the warning
            warning=function(w) {
              message('Check your internet connection')
              return(NULL)
            }
  )

  if(length(res)==0)
    return(NULL)

  engine <- res[1,strsplit(V14,'_')][1]
  market <- res[1,strsplit(V14,'_')][2]

  if(engine=='stock' & market!='bonds')
    market <- 'shares'

  board  <- res[1,V15]

  pos <- 0
  baseurl   <- 'https://iss.moex.com/iss/engines/'
  marketurl <-paste0(engine,'/markets/',market,'/boards/',board)

  url <- paste0(baseurl,
                marketurl,
                '/securities/',
                Symbol.name,
                '/candles.csv?from=',
                from,
                '&till=',
                to,
                '&interval=',
                interval,
                '&start=',pos)

  dt <- data.table()

  tryCatch(tdt <- fread(url),
    #if an error occurs, tell me the error
    #if an error occurs, tell me the error
    error=function(e) {
      message('Server of MOEX not response - try later')
      return(NULL)
      #print(e)
    }
  )



  if(nrow(tdt)==0)
    return(NULL)

  tdt[,timestamp:=as.POSIXct(begin)]

  maxPB <-as.numeric(difftime(as.POSIXct(to),
                              as.POSIXct(from),
                              units = 'secs'))
  dt<-rbind(dt, tdt)
    while(nrow(tdt)!=0){
      pos <- pos+nrow(tdt)
      url <- paste0(baseurl,
                    marketurl,
                    '/securities/',
                    Symbol.name,
                    '/candles.csv?from=',
                    from,
                    '&till=',
                    to,
                    '&interval=',interval,
                    '&start=',pos)
      tryCatch(tdt <- fread(url),
               #if an error occurs, tell me the error
               error=function(e) {
                 message('Server of MOEX not response - try later')
                 return(NULL)
                 #print(e)
               },
               #if a warning occurs, tell me the warning
               warning=function(w) {
                 message('Check your internet connection')
                 return(NULL)
               }
      )

      if(nrow(tdt)){
        tdt[,timestamp:=as.POSIXct(begin)]
        dt<-rbind(dt, tdt)
      }
    }
  Symbol.name <-toupper(gsub('\\^','',Symbol.name))
  if(auto.assign)
    assign(Symbol.name,dt,env)
  }
    if(auto.assign)
      return(Symbols)
    return(dt)
}
