"getSymbols.Finam" <-
  function(Symbols,env,return.class='xts',index.class='Date',
           from='2007-01-01',
           to=Sys.Date(),
           adjust=FALSE,
           period='day',
           market=NULL,
           ...)
  {
    importDefaults("getSymbols.Finam")
    this.env <- environment()
    for(var in names(list(...))) {
      # import all named elements that are NON formals
      assign(var, list(...)[[var]], this.env)
    }
    
    default.return.class <- return.class
    default.from <- from
    default.to <- to
    
    if(missing(verbose)) verbose <- FALSE
    if(missing(auto.assign)) auto.assign <- FALSE
    
    p <- 0
    
    if ("tick" == period) p <- 1
    if ("1min" == period) p <- 2
    if ("5min" == period) p <- 3
    if ("10min" == period) p <- 4
    if ("15min" == period) p <- 5
    if ("30min" == period) p <- 6
    if ("hour" == period) p <- 7
    if ("day" == period) p <- 8
    if ("week" == period) p <- 9
    if ("month" == period) p <- 10
    
    if (p==0) {
      message(paste("Unkown period ", period))
    }	 
    
    if (!exists("finam.stock.list")){
      finam.stock.list <- getSymbolList(src='Finam')
      assign('finam.stock.list', finam.stock.list, env)
    }
    fr <- NaN
    for(i in 1:length(Symbols)) {
      
      return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
      return.class <- ifelse(is.null(return.class),default.return.class,
                             return.class)
      from <- getSymbolLookup()[[Symbols[[i]]]]$from
      from <- if(is.null(from)) default.from else from
      to <- getSymbolLookup()[[Symbols[[i]]]]$to
      to <- if(is.null(to)) default.to else to
      
      from.y <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][1])
      from.m <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][2])-1
      from.d <- as.numeric(strsplit(as.character(as.Date(from,origin='1970-01-01')),'-',)[[1]][3])
      to.y <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][1])
      to.m <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][2])-1
      to.d <- as.numeric(strsplit(as.character(as.Date(to,origin='1970-01-01')),'-',)[[1]][3])
      
      Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
      Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
      if(verbose) cat("downloading ",Symbols.name,".....\n\n")
      
      
      if(!is.null(market))
      {
        finam.stock <- finam.stock.list[as.character(finam.stock.list$Symbol) == Symbols.name,]
        Symbols.id <- finam.stock[finam.stock$Market == market,]$Id
      }
      if(is.null(market))
      {
        finam.stock <- finam.stock.list[as.character(finam.stock.list$Symbol) == Symbols.name,]
        Symbols.id <- finam.stock[which.min(finam.stock$Market),]$Id
        market <- finam.stock[which.min(finam.stock$Market),]$Market
      }
      
      if (length(Symbols.id)==0){
        if (verbose)
          cat("Don't know about",Symbols[[i]],"\n\n")
        next
      }
      
      finam.HOST <- 'export.finam.ru'
      finam.URL <- "/table.csv?d=d&f=table&e=.csv&dtf=1&tmf=1&MSOR=0&sep=1&sep2=1&at=1&"
      
      stock.URL <- paste(finam.URL,
                         "p=", p,
                         "&market=",1,
                         "&em=",Symbols.id,
                         "&df=",from.d,
                         "&mf=",from.m,
                         "&yf=",from.y,
                         "&dt=",to.d,
                         "&mt=",to.m,
                         "&yt=",to.y,
                         "&cn=",Symbols.name,
                         sep='')
      if (verbose) cat(stock.URL);
      tmp <- tempfile()
      if (p==1){
        stock.URL <- paste('http://', finam.HOST, stock.URL, '&datf=6' , sep='')
      }else {
        stock.URL <- paste('http://', finam.HOST, stock.URL, '&datf=1' , sep='')
      }
      download.file(stock.URL, destfile=tmp, quiet=!verbose)
      fr <- read.csv(tmp, as.is=TRUE, colClasses="character")
      unlink(tmp)
      
      if(verbose) cat("done.\n")
      if (p==1){
        if(verbose) print(head(fr))
        fr <- xts(apply(as.matrix(fr[,(5:6)]),2, as.numeric), as.POSIXct(strptime(paste(fr[,3],fr[,4]), "%Y%m%d %H%M%S")),
                  src='finam',updated=Sys.time())
        colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                              c('Close','Volume'),
                              sep='.')
      }else if (p>7) {
        fr <- xts(apply(as.matrix(fr[,(5:9)]),2, as.numeric), as.Date(strptime(fr[,3], "%Y%m%d")),
                  src='finam',updated=Sys.time())
        colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                              c('Open','High','Low','Close','Volume'),
                              sep='.')
      }else {
        fr <- xts(apply(as.matrix(fr[,(5:9)]),2,as.numeric), as.POSIXct(strptime(paste(fr[,3],fr[,4]), "%Y%m%d %H%M%S")),
                  src='finam',updated=Sys.time())
        colnames(fr) <- paste(toupper(gsub('\\^','',Symbols.name)),
                              c('Open','High','Low','Close','Volume'),
                              sep='.')
      }
      
      fr <- convert.time.series(fr=fr,return.class=return.class)
      if(is.xts(fr) && p>7)
        indexClass(fr) <- index.class
      
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
    
    return(fr)
  }

"getSymbols.Investing" <-
  function(Symbols,env,return.class='xts',index.class='Date',
           from='2007-01-01',
           to=Sys.Date(),
           adjust=FALSE,
           period='day',
           ...)
  {
    importDefaults("getSymbols.Investing")
    this.env <- environment()
    for(var in names(list(...))) {
      # import all named elements that are NON formals
      assign(var, list(...)[[var]], this.env)
    }
    
    default.return.class <- return.class
    default.from <- from
    default.to <- to
    
    if(missing(verbose)) verbose <- FALSE
    if(missing(auto.assign)) auto.assign <- FALSE
    
    url = 'https://www.investing.com/instruments/HistoricalDataAjax'
    start_date = from
    end_date = to
    # information for request
    for(Symbol in Symbols) {
      
      headers = add_headers('Host' = 'www.investing.com','Origin' = 'https://www.investing.com','Referer' = 'https://www.investing.com/dividends-calendar/','X-Requested-With' = 'XMLHttpRequest','Content-Type' = 'application/x-www-form-urlencoded','Connection' = 'keep-alive','Accept-Language' = 'en-US,en;q=0.9,fr;q=0.8,ja;q=0.7,es;q=0.6','Accept-Encoding' = 'Encoding:gzip, deflate','Accept' = '*/*','User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.108 Safari/537.36')
      data <- paste0(data = 'curr_id=',getInvesting_id(Symbol),'&header=',Symbol,'+Historical+Data&st_date=',format(as.Date(start_date),'%m/%d/%Y'),'&end_date=',format(as.Date(end_date),'%m/%d/%Y'),'&interval_sec=Daily&sort_col=date&sort_ord=DESC&action=historical_data')
      # request
      r <- POST(url,headers,body = data,encode = "raw")
      if(status_code(r) == 200)
      {
        cr <- content(r,as = 'text')
        cr <- gsub(',','',cr)
        Records <- readHTMLTable(cr)$curr_table
        if(ncol(Records)>2)
        {
          current_locale <- Sys.getlocale("LC_TIME")
          Sys.setlocale("LC_TIME", "C") 
          names(Records)[1:5] <- c('Date','Close','Open','High','Low')
          Records <- Records[,c(1,3,4,5,2,6)]
          Records$Date <- as.Date(as.character(Records$Date),'%b %d %Y')
          Records$Open <- ifelse(grepl('K',Records$Open),1000*as.numeric(as.character(gsub('K','',as.character(Records$Open)))),as.numeric(as.character(Records$Open)))
          Records$High <- ifelse(grepl('K',Records$High),1000*as.numeric(as.character(gsub('K','',Records$High))),as.numeric(as.character(Records$High)))
          Records$Low <- ifelse(grepl('K',Records$Low),1000*as.numeric(as.character(gsub('K','',Records$Low))),as.numeric(as.character(Records$Low)))
          Records$Close <- ifelse(grepl('K',Records$Close),1000*as.numeric(as.character(gsub('K','',Records$Close))),as.numeric(as.character(Records$Close)))
          
          if(ncol(Records)>5)
            if(names(Records)[6] == "Vol.")
            {
              Records$Volume <- as.numeric(as.character(Records[,6]))
              Records$Volume[grep('K',Records[,6])] <- 1000*as.numeric(gsub('K','',Records[grep('K',Records[,6]),6]))
              Records$Volume[grep('M',Records[,6])] <- 1000000*as.numeric(gsub('M','',Records[grep('M',Records[,6]),6]))
              Records[,6] <- Records$Volume
              Records <- xts(Records[,2:6],order.by = Records[,1])
            }
          if(length(grep('Vol.',names(Records))) == 0)
            Records <- xts(Records[,2:5],order.by = Records[,1])
          Sys.setlocale("LC_TIME", current_locale) 
        }
      }
      if(status_code(r) != 200)
      {
        Records <- 'cannot connect to server'
      }
    }
    if(auto.assign)
      return(Records)
    return(Records)
  }

"getSymbols.Poloniex" <- function 
(Symbols,env,return.class='xts',index.class='Date',
 from='2007-01-01',
 to=Sys.Date(),
 adjust=FALSE,
 period='day',
 ...)
{
	importDefaults("getSymbols.Poloniex"); #rewrite default values if specified by setDefaults
	local_env <- environment()
	for(var in names(list(...))) {
		# import all named elements that are NON formals
		assign(var, list(...)[[var]], local_env)
	}

	options(warn = -1)
	default.return.class <- return.class
	default.from <- from
	default.to <- to

	if(missing(verbose)) verbose <- TRUE
	if(missing(auto.assign)) auto.assign <- FALSE
	
	Polo.period <- 0
	Polo.max_candles <- 1000
	Polo.from <- as.numeric(as.POSIXct(from)) #convert to UNIX timestamp
	Polo.to <- as.numeric(as.POSIXct(to)) #convert to UNIX timestamp

	Polo.downloadUrl <- 'https://poloniex.com/public?command=returnChartData'
	
	switch(period, #select candle period according to data frequency
		 '5min' = {
		   Polo.period <- 300
		 },
		 '15min' = {
		   Polo.period <- 900
		 },
		 '30min' = {
		   Polo.period <- 1800
		 },
		 '2hours' = {
		   Polo.period <- 7200
		 },
		 '4hours' = {
		   Polo.period <- 14400
		 },
		 'day' = {
		   Polo.period <- 86400
		 }
	)

	#example API usage
	#"https://poloniex.com/public?command=returnChartData&currencyPair=USDT_BTC&period=7200&start=1468711470&end=1468757470"
	
	for(i in 1:length(Symbols)) {
		Polo.url <- paste(Polo.downloadUrl,
                        "&currencyPair=", Symbols[[i]],
                        "&period=", Polo.period,
                        "&start=", Polo.from,
                        "&end=", Polo.to, sep="")
		tmp <- tempfile()		
		download.file(Polo.url, destfile = tmp, quiet = TRUE) #get JSON object
		rawdata <- readLines(tmp) #read raw data from file
		if(substr(rawdata, 3, 7) == 'error') {
		  
		  stop(paste('Error!', substr(rawdata, 11, nchar(rawdata) - 2)))
		}
		
		#parsing JSON object
		str_obs <- strsplit(rawdata, split = "},{", fixed = TRUE)
		str_obs[[1]][1] <- substr(str_obs[[1]][1], 3, nchar(str_obs[[1]][1]))
		temp <- str_obs[[1]][length(str_obs[[1]])]
		substr(temp, 1, nchar(temp)-2) -> str_obs[[1]][length(str_obs[[1]])]
		
		ticker_header <- c('Date', 'High', 'Low', 'Open', 'Close', 'Volume')
		ticker_length <- length(ticker_header)
		lst <- list()
		
		for(j in 1:length(str_obs[[1]])) {
		  str <- str_obs[[1]][j]
		  str_par <- strsplit(str, ",", fixed = TRUE)
		  vec_row <- c()
		  for(k in 1:ticker_length){
		    vec_row <- append(vec_row, values = substr(str_par[[1]][k], 4+nchar(ticker_header[k]), nchar(str_par[[1]][k])))
		  }
		  
		  lst[[length(lst)+1]] <- vec_row
		}
		
		res <- do.call(rbind.data.frame, lst)
		names(res) <- ticker_header
		
		nts <- xts(apply(res[,c(4,2,3,5:length(res)) ], 2, as.numeric),
		           as.POSIXct(as.numeric(as.character(res[, 1])), origin="1970-01-01", tz ="GMT"))
		colnames(nts) <- paste(toupper(gsub('\\^','',Symbols[[i]])),
		                      c('Open','High','Low','Close','Volume'),
		                      sep='.')
		
		fr <- convert.time.series(fr=nts, return.class=return.class)
		
		Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))
		
		if(auto.assign){
		  assign(Symbols[[i]], fr, env)
		}
	}
	
	if(auto.assign){
		return(Symbols)
	}

	return(fr)
}

#function retrives data from alortrade broker
"getSymbols.Alor" <- function #S3 function (Alor is a class of first argument)
(Symbols,env,return.class='xts',index.class='Date',
 from='2007-01-01',
 to=Sys.Date(),
 adjust=FALSE,
 period='day',
 ...)
{
  importDefaults("getSymbols.Alor"); #rewrite default values if specified by setDefaults
  local_env <- environment()
  for(var in names(list(...))) {
    # import all named elements that are NON formals
    assign(var, list(...)[[var]], local_env)
  }
  
  options(warn = -1)
  default.return.class <- return.class
  default.from <- from
  default.to <- to
  
  if(missing(verbose)) verbose <- TRUE
  if(missing(auto.assign)) auto.assign <- FALSE
  board <- "MICEX" #data is grabbed from Moscow Exchange by default
  
  Alor.period <- 0
  Alor.max_candles <- 1000
  Alor.from <- as.POSIXct(from)
  Alor.to <- as.POSIXct(to)
  
  Alor.downloadUrl <- 'http://history.alor.ru/?'
  
  switch(period, #select candle period according to data frequency
         'tick' = {
           Alor.period <- 0
         },
         '1min' = {
           Alor.period <- 1
         },
         '5min' = {
           Alor.period <- 5
         },
         '10min' = {
           Alor.period <- 10
         },
         '15min' = {
           Alor.period <- 15
         },
         '20min' = {
           Alor.period <- 20
         },
         '30min' = {
           Alor.period <- 30
         },
         'hour' = {
           Alor.period <- 60
         },
         'day' = {
           Alor.period <- 1440
         }
  )
  
  #example of request
  #http://history.alor.ru/?board=MICEX&ticker=LKOH&period=5&from=2016-03-21&to=2016-07-12&bars=5
  
  result <- as.null()
  
  for(i in 1:length(Symbols)) {
    #building request to a REST service
    #bypassing 1000 obs restriction
    repeat
    {
      Alor.url <- paste(Alor.downloadUrl, "board=", board,
                        "&ticker=", Symbols[[i]],
                        "&period=", Alor.period,
                        "&from=", Alor.from,
                        "&to=", Alor.to,
                        "&bars=",Alor.max_candles)
      
      temp <- tempfile()
      download.file(Alor.url, destfile = temp, quiet = TRUE) #request to a service
      #reading result as a table
      res <- read.table(temp, header = FALSE, col.names = c('Date', 'Time', 'Open', 'High', 'Low', 'Close', 'Volume'))
      res$Date <- paste(res$Date, " ", res$Time) #merge time a date columns
      res[,!(names(res) == 'Time')] #remove time column
      unlink(temp) #release resource
      
      #if responce returns same date break a loop
      if(!is.null(result)){
        if(result$Date[1] == res$Date[nrow(res)]){
          break;
        }
      }
      
      Alor.to <- as.POSIXct(res$Date[nrow(res)]) + Alor.period * 60 #update a date of observation
      result <- if(is.null(result)) res[order(res$Date), ] else merge(result, res, all=TRUE)
      
      Sys.sleep(0.01) #delay between requests
    }
    
    #build xts time series
    newts <- xts(apply(result[,3:length(result)], 2, as.numeric) , as.POSIXct(result[, 1]))
    fr <- convert.time.series(fr=newts,return.class=return.class)
    
    Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))
    
    if(auto.assign){
      assign(Symbols[[i]], fr, env)
    }
  }
  
  if(auto.assign){
    return(Symbols)
  }
  
  return(fr)
}



"getSymbols.Mfd" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
     importDefaults("getSymbols.Mfd")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }

     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- FALSE
     if(missing(auto.assign)) auto.assign <- FALSE

     p <- -1

	 if ("tick" == period) p <- 0
     if ("1min" == period) p <- 1
     if ("5min" == period) p <- 2
     if ("10min" == period) p <- 3
     if ("15min" == period) p <- 4
     if ("30min" == period) p <- 5
     if ("hour" == period) p <- 6
     if ("day" == period) p <- 7
     if ("week" == period) p <- 8
     if ("month" == period) p <- 9

     if (p==-1) {
        message(paste("Unkown period ", period))
     }
    for(i in 1:length(Symbols)) {
   
	format(as.Date(from),"%d.%m.%Y")->mfd.from
	format(as.Date(to),"%d.%m.%Y")->mfd.to
	Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
    Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
	 data("tickers")
	 SYMBOL.GROUP<- tickers[which(tickers[,4]==Symbols),1]
	 SYMBOL.ID<-tickers[which(tickers[,4]==Symbols),3]
	 
       if (length(SYMBOL.ID)==0){
           if (verbose)
                cat("Don't know about",Symbols[[i]],"\n\n")
           next
       }

	 
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
	
       fr <- convert.time.series(fr=fr,return.class=return.class)
       if(is.xts(fr) && p>7)
         indexClass(fr) <- index.class

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

#This one is taken from quanmod package since it's not available through the API
"convert.time.series" <-
function (fr, return.class)
{
    if ("quantmod.OHLC" %in% return.class) {
        class(fr) <- c("quantmod.OHLC", "zoo")
        return(fr)
    }
    else if ("xts" %in% return.class) {
        return(fr)
    }
    if ("zoo" %in% return.class) {
        return(as.zoo(fr))
    }
    else if ("ts" %in% return.class) {
        fr <- as.ts(fr)
        return(fr)
    }
    else if ("data.frame" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("matrix" %in% return.class) {
        fr <- as.data.frame(fr)
        return(fr)
    }
    else if ("its" %in% return.class) {
        if ("package:its" %in% search() || suppressMessages(require("its",
            quietly = TRUE))) {
            fr.dates <- as.POSIXct(as.character(index(fr)))
            fr <- its::its(coredata(fr), fr.dates)
            return(fr)
        }
        else {
            warning(paste("'its' from package 'its' could not be loaded:",
                " 'xts' class returned"))
        }
    }
    else if ("timeSeries" %in% return.class) {
        if ("package:fSeries" %in% search() || suppressMessages(require("fSeries",  quietly = TRUE))) {
            fr <- timeSeries(coredata(fr), charvec = as.character(index(fr)))
            return(fr)
        }
        else {
            warning(paste("'timeSeries' from package 'fSeries' could not be loaded:", " 'xts' class returned"))
        }
    }
}


"getInvesting_id" <- function(Symbol='AAPL')
{
  url = 'https://www.investing.com/search/service/search'
  
  headers = add_headers('Host' = 'www.investing.com',
                        'Origin' = 'https://www.investing.com',
                        'Referer' = 'https://www.investing.com/search/service/search',
                        'X-Requested-With' = 'XMLHttpRequest',
                        'Content-Type' = 'application/x-www-form-urlencoded',
                        'Connection' = 'keep-alive',
                        'Accept-Language' = 'en-US,en;q=0.9,fr;q=0.8,ja;q=0.7,es;q=0.6',
                        'Accept-Encoding' = 'Encoding:gzip, deflate',
                        'Accept' = '*/*',
                        'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.108 Safari/537.36')
  
  data = paste0('search_text=',Symbol,'&term=',Symbol,'&country_id=0&tab_id=All')
  
  r <- POST(url,headers,body = data,encode = "raw")
  
  if(status_code(r) == 200)
  {
    cr <- content(r,as = 'text')
    json_data <- fromJSON(cr)
  }
  inv_id <- json_data$All$pair_ID[which.max(json_data$All$popularity_rank)]
  return(inv_id)
}


"select.hours" <-
function(data, hour){
    return(data[format(index(data), format="%H")==hour])
}


