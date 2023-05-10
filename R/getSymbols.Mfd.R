#' @title Get financial data from Mfd.ru
#'
#' @description This function retrieves financial data from Mfd.ru
#'
#' @param Symbols character vector of symbols to retrieve
#' @param env environment where the data will be assigned
#' @param from a character string representing the starting date in 'YYYY-MM-DD' format. Default is '2007-01-01'.
#' @param to a character string representing the ending date in 'YYYY-MM-DD' format. Default is the current system date.
#' @param adjust a logical specifying whether to adjust for dividends and stock splits. Default is FALSE.
#' @param period a character string specifying the period of the data to retrieve. Must be one of 'tick', '1min', '5min', '10min', '15min', '30min', 'hour', 'day', 'week', or 'month'. Default is 'day'.
#' @param verbose a logical specifying whether to print progress messages. Default is TRUE.
#' @param auto.assign a logical specifying whether to automatically assign the retrieved data to the symbols specified in the Symbols argument. Default is FALSE.
#' @param ... additional arguments passed to getSymbols.Mfd
#' @return If auto.assign is TRUE, the function returns the Symbols argument with the retrieved data assigned to each symbol. If auto.assign is FALSE, the function returns an xts object.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @export

"getSymbols.Mfd" <-
  function(Symbols,env = globalenv(),
           from='2007-01-01',
           to=Sys.Date(),
           adjust=FALSE,
           period='day',
           verbose=TRUE,
           auto.assign=FALSE,
           ...)
  {
    V3 <- NULL
    p <- -1
    periods = c('tick'=0, '1min'=1, '5min'=2, '10min'=3,'15min'=4,'30min'=5,'hour'=6,'day'=7,'week'=8,'month'=9)
    p = periods[period]

    if (p==-1) {
      message(paste("Unkown period ", period))
    }
    for(i in 1:length(Symbols)) {

      format(as.Date(from),"%d.%m.%Y") -> mfd.from
      format(as.Date(to),"%d.%m.%Y") -> mfd.to
      Symbols.name <- Symbols[i]
      if (!exists("symbol_list_MFD"))
      {
        symbol_list_MFD = NULL
        getSymbolList(src='Mfd',auto.assign = TRUE)
        symbol_list_MFD = getSymbolList(src='Mfd',auto.assign = FALSE)
      }

      SYMBOL.GROUP<- symbol_list_MFD[V3==Symbols.name,1]
      SYMBOL.ID<-symbol_list_MFD[V3==Symbols.name,2]



      mfd.URL<-"http://mfd.ru/export/handler.ashx/Data.txt?"

      stock.URL <- paste(mfd.URL,
                         "TickerGroup=",SYMBOL.GROUP,
                         "&Tickers=",SYMBOL.ID,
                         "&Alias=false&Period=",p,
                         "&timeframeValue=1&timeframeDatePart=day&StartDate=",mfd.from,"&EndDate=",mfd.to,
                         "&SaveFormat=0&SaveMode=0&FileName=Date18112013_23112013.txt&FieldSeparator=%253b&DecimalSeparator=.&DateFormat=yyyyMMdd&TimeFormat=HHmmss&DateFormatCustom=&TimeFormatCustom=&AddHeader=true&RecordFormat=0&Fill=false",sep="")
      tmp <- tempfile()
      download.file(stock.URL, destfile=tmp,quiet=TRUE)
      fr <- read.table(tmp, sep=";",header=TRUE)
      unlink(tmp)
      if (p<7) {


        paste("0",as.character(fr[fr[,4]<100000 & fr[,4]>=10000,4]),sep="")->fr[fr[,4]<100000 & fr[,4]>=10000,4]
        paste("00",(fr[as.double(fr[,4])<10000 & as.double(fr[,4])>0,4]),sep="")->fr[as.double(fr[,4])<10000 & as.double(fr[,4])>0,4]
        paste("00000",(fr[fr[,4]=='0',4]),sep="")->fr[fr[,4]=='0',4]
        fr <- xts(apply(as.matrix(fr[,(5:10)]),2, as.numeric), as.POSIXct(strptime(paste(fr[,3],fr[,4]), "%Y%m%d %H%M%S")),src='mfd',updated=Sys.time())
        colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                              c('Open','High','Low','Close','Volume','OPEN_INTEREST'),
                              sep='.')
      }
      if (p>=7) {

        fr <- xts(apply(as.matrix(fr[,(5:10)]),2, as.numeric), as.Date(strptime(fr[,3], "%Y%m%d")),src='mfd',updated=Sys.time())
        colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                              c('Open','High','Low','Close','Volume','OPEN_INTEREST'),
                              sep='.')}

      Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))
      if(auto.assign)
        assign(Symbols[[i]],fr,env)
      if(i >= 5 && length(Symbols) > 5) {
        message("pausing 1 second between requests for more than 5 symbols")
        Sys.sleep(1)
      }
    }
    if(auto.assign)
      return(Symbols)

    if(exists('fr'))
      return(fr)
  }
