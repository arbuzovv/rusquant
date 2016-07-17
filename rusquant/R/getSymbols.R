#function retrives data from poloniex through its public API
"getSymbols.Poloniex" <- function #S3 function (Poloniex is a class of first argument)
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
		
		ticker_header <- c('date', 'high', 'low', 'open', 'close', 'volume')
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
		
		nts <- xts(apply(res[,2:length(res)], 2, as.numeric),
		           as.POSIXct(as.numeric(as.character(res[, 1])), origin="1970-01-01", tz ="GMT"))
		
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



"getSymbols.rogov" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
   importDefaults("getSymbols.rogov")
     this.env <- environment()
     for(var in names(list(...))) {
        # import all named elements that are NON formals
        assign(var, list(...)[[var]], this.env)
     }
	 options(warn = -1)
     default.return.class <- return.class
     default.from <- from
     default.to <- to

     if(missing(verbose)) verbose <- TRUE
     if(missing(auto.assign)) auto.assign <- FALSE

     p <- 0
     if ("hour" == period) 
		 {
		 p <- 'Hourly' 
		 limit<-16000
		 }
	if ("day" == period) 
		 {
		 p <- 'Daily' 
		 limit<-16000
		 }
    if ("week" == period) 
		{
		p <- 'Weekly' 
		limit<-10000
		}
    if ("month" == period)
		{
		p <- 'Monthly' 
		limit<-1000
		}
	if ("year" == period)
		{
		p <- 'Annually'
		limit<-100
		}
    if (p==0) 
		 {
			message(paste("Unkown period ", period))
		 }
  
	format(as.Date(from),"%m.%d.%Y")->rogov.from
	format(as.Date(to),"%m.%d.%Y")->rogov.to
for(i in 1:length(Symbols)) {	
	rogov.URL<-"http://www.rogovindex.com/Quote/searchentries?rogovindex.period="	
	stock.URL <- paste(rogov.URL,p,"&limit=",limit,"&RegionFromDate=",rogov.from,"&regiontodate=",rogov.to,sep="")
	tmp <- tempfile()
    download.file(stock.URL, destfile=tmp,quiet=TRUE)
    fr <- read.table(tmp, sep="{",header=FALSE)	 
	as.character(unlist(fr[3:length(fr)]))->fr
	gsub("ValueDateString:", "", fr)->fr
	gsub("BaseValue:", "", fr)->fr
	gsub("FValue:", "", fr)->fr
	gsub("BValue:", "", fr)->fr
	gsub("RValue:", "", fr)->fr
	gsub("YValue:", "", fr)->fr
	gsub("},", "", fr)->fr
	gsub("}]}", "", fr)->fr		
	t(as.data.frame(strsplit(fr,",")))->fr
 
    unlink(tmp)	 	 
    
	if(p=='Hourly')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.POSIXct(strptime(fr[,1], "%m/%d/%Y %I:%M:%S %p")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		}
	if(p=='Monthly')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.Date(strptime(paste("01/",fr[,1],sep=""), "%d/%m/%Y")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		} 
	if(p=='Annually')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.Date(strptime(paste("01/01/",fr[,1],sep=""), "%d/%m/%Y")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		} 	 
	 
    if(p=='Daily' | p=='Weekly')       
		{
		fr <- xts(apply(as.matrix(fr[,2:6]),2, as.numeric), as.Date(strptime(fr[,1], "%m/%d/%Y")),src='rogov',updated=Sys.time())
		colnames(fr) <- c('Base Value','F Value','B Value','R Value','Y Value')
		}

       fr <- convert.time.series(fr=fr,return.class=return.class)
     #  if(is.xts(fr))
     #    indexClass(fr) <- index.class

       Symbols[[i]] <-toupper(gsub('\\^','',Symbols[[i]]))
       if(auto.assign)
         assign(Symbols[[i]],fr,env)
       if(i >= 5 && length(Symbols) > 5) {
         message("pausing 1 second between requests for more than 5 symbols")
         Sys.sleep(1)
       }
     }
	 print('The RogovIndex© indices and tools are proprietary to and distributed by Mikhail Rogov.' )
	 print('All content of the Rogov Index© is proprietary to Mikhail Rogov Terms and Conditions of Access Provider at our discretion,')
	 print('provide you with services including, but not restricted to, RogovIndex© indices and tools. ')
	 print('You agree to comply with the conditions imposed on your use of the services, as set out in these Terms and Conditions of Access and elsewhere in our services. ')
	 print('These services may be outside our control or provided by a third party in which in case we cannot take responsibility for their content, or for any delays,')
	 print('interruptions or errors in the provisions of these additional services, provided we have exercised reasonable care and diligence in the selection of such ')
	 print('providers.Certain data accessible on our services is the intellectual property of us. The data is protected by copyright and other intellectual laws ')
	 print('and all ownership rights remain with us. You may only use the data retrieved from our services for your own purposes while accessing our services. ')
	 print('Such use will be in accordance with these Terms and Conditions of Access and the requirements set out elsewhere on our services. You may not copy, distribute')
	 print('or redistribute the data, including by caching, framing or similar means or sell, resell, re-transmit or otherwise make the data retrieved from our services')
	 print('available in any manner to any third party.The data is provided "as is." We or any third party shall not be liable to you or any third party for any loss or damage,')
	 print('direct, indirect or consequential, arising from (i) any inaccuracy or incompleteness in, or delays, interruptions, errors or omissions in the delivery of the data or any ')
	 print('other information supplied to you through our services or (ii) any decision made or action taken by you or any third party in reliance upon the data. Third party nor we ')
	 print('shall be liable for loss of business revenues, lost profits or any punitive, indirect, consequential, special or similar damages whatsoever, whether in contract, tort or ')
	 print('otherwise, even if advised of the possibility of such damages incurred by you or any third party.Where the information consists of pricing or performance data, the data ')
	 print('contained therein has been obtained from sources believed reliable. Data computations are not guaranteed by any information service provider, third party or us or any affiliates')
	 print('and may not be complete. Neither any information service provider, third party or us give any warranties, as to the accuracy, adequacy, quality or fitness, timeless, ')
	 print('non-infringement, title, of any information for a particular purpose or use and all such warranties are expressly excluded to the fullest extent that such warranties ')
	 print('may be excluded by law. You bear all risk from any use or results of using any information.You are responsible for validating the integrity of any information received ')
	 print('over the Internet.Transmission may be subject to arbitrary delays beyond our control, which may delay the provision of our services and the execution of your orders.')
	 print('You acknowledge that neither any information service provider, third party nor we will be liable to you or any third party for any losses arising from such delay. ')
	 print('In no event will any information provider, third party or we, be liable for any consequential loss including but not limited to special, incidental, direct or indirect ')
	 print('damages resulting from delay or loss of use of our services. We are not responsible for any damage to your computer, software, modem, telephone or other property resulting ')
	 print('from your use of our services.')
     if(auto.assign)
       return(Symbols)
	
	
    return(fr)
}

"getSymbols.Finam" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
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
     finam.HOST <- '195.128.78.52'
     finam.URL <- "/table.csv?d=d&market=1&f=table&e=.csv&dtf=1&tmf=1&MSOR=0&sep=1&sep2=1&at=1&"

     if (!exists("finam.stock.list")){
        finam.stock.list <- loadStockList()
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
       Symbols.id <- finam.stock.list[Symbols.name]

       if (is.na(Symbols.id)){
           if (verbose)
                cat("Don't know about",Symbols[[i]],"\n\n")
           next
       }

       stock.URL <- paste(finam.URL,
                           "p=", p,
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
           lts <-  http.get(finam.HOST, paste(stock.URL, '&datf=6', sep=''),  referer='http://www.finam.ru/analysis/export/default.asp', verbose=verbose)
           write(lts, file=tmp)
       }else {
           stock.URL <- paste('http://', finam.HOST, stock.URL, '&datf=1' , sep='')
           download.file(stock.URL, destfile=tmp, quiet=!verbose)
       }
       fr <- read.csv(tmp, as.is=TRUE, colClasses="character")
       unlink(tmp)

       if(verbose) cat("done.\n")
       if (p==1){
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


"getSymbols.mfd" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
     importDefaults("getSymbols.mfd")
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

"loadStockList" <-
function (verbose = FALSE){
    stocklist.URL = 'http://www.finam.ru/cache/icharts/icharts.js'
    tmp <- tempfile()
    download.file(stocklist.URL, destfile=tmp,quiet=!verbose)
    fr <- readLines(con = tmp, warn=FALSE)
    unlink(tmp)
    ids <- sub("var .*?= \\[", "", fr[1])
    ids <- sub("\\];", "", ids)
    ids <- strsplit(ids, ",")
	
	markets <- sub("var .*?= \\[", "", fr[4])
    markets <- sub("\\];", "", markets)
    markets <- strsplit(markets, ",")

    names <- sub("var .*?= \\[", "", fr[3])
    names <- sub("\\];", "", names)
    names <- gsub("'", "", names)
    names <- strsplit(names, ",")
	names[[1]]->names
	names[-(11414)]->names
    res <- unlist(ids)
	
	data<-data.frame(names,res,markets)
	data[data[,3]!=3,]->data
	rbind(data[(data[,1]%in%data[data[,3]==1,1]) & data[,3]==1,],data[!(data[,1]%in%data[data[,3]==1,1]),])->data
	data[,2]->res
    names(res) <- data[,1]
    return(res)
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

"getSymbols.Forts" <-
function(Symbols,env,return.class='xts',index.class='Date',
         from='2007-01-01',
         to=Sys.Date(),
         adjust=FALSE,
         period='day',
         ...)
{
     importDefaults("getSymbols.Forts")
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

     forts.URL <- "http://www.rts.ru/ru/forts/contractresults-exp.html?"

     for(i in 1:length(Symbols)) {

       return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
       return.class <- ifelse(is.null(return.class),default.return.class,
                              return.class)
       from <- getSymbolLookup()[[Symbols[[i]]]]$from
       from <- if(is.null(from)) default.from else from
       to <- getSymbolLookup()[[Symbols[[i]]]]$to
       to <- if(is.null(to)) default.to else to

       from.f <- format(as.Date(from,origin='1970-01-01'), '%Y%m%d')
       to.f <- format(as.Date(to,origin='1970-01-01'), '%Y%m%d')

       Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
       Symbols.name <- ifelse(is.null(Symbols.name),Symbols[[i]],Symbols.name)
       if(verbose) cat("downloading ",Symbols.name,".....\n\n")

       tmp <- tempfile()
       stock.URL <- paste(forts.URL,
                           "day1=", from.f,
                           "&day2=", to.f,
                           "&isin=",gsub(' ', '%20', Symbols.name),
                           sep='')

       download.file(stock.URL, destfile=tmp, quiet=!verbose)

       fr <- read.csv(tmp, as.is=TRUE, skip=1)
       unlink(tmp)

       if(verbose) cat("done.\n")

      fr <- xts(as.matrix(cbind(fr[,(4:7)], fr[,12], fr[,14]) ), as.Date(strptime(fr[,1], "%d.%m.%Y")),
                src='forts',updated=Sys.time())

       colnames(fr) <- paste(toupper(gsub('[ -.]','',Symbols.name)),
                             c('Open','High','Low','Close', 'Volume', 'Positions'),
                             sep='.')

       fr <- convert.time.series(fr=fr,return.class=return.class)
       if(is.xts(fr))
         indexClass(fr) <- index.class

       Symbols[[i]] <-toupper(gsub('[ -.]','',Symbols[[i]]))
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


"select.hours" <-
function(data, hour){
    return(data[format(index(data), format="%H")==hour])
}


http.get <- function(host, path, port=80, referer="", verbose=FALSE) {

  if(missing(path))
    path <- "/"
  if(missing(host))
    stop("No host URL provided")

  header <- NULL
  header <- c(header,paste("GET ", path, " HTTP/1.0\r\n", sep=""))
  header <- c(header,"User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64; rv:6.0.2) Gecko/20100101 Firefox/6.0.2\r\n")
  header <- c(header,"Accept: */*\r\n")
  header <- c(header,"Accept-Encoding: deflate\r\n")
  header <- c(header,paste("Referer: ", referer, "\r\n", sep=""))

  request <- paste(header, sep="", collapse="")

  if (verbose) {
    cat("Sending HTTP GET request to ", host, ":", port, "\n")
    cat(request, "\n")
  }

  con <- socketConnection(host=host, port=port, open="w+", blocking=TRUE, encoding="UTF-8")

  on.exit(close(con))

  writeLines(request, con)

  response <- list()
  response$status <- readLines(con, n=1)
  if (verbose) {
    write(response$status, stderr())
    flush(stderr())
  }
  response$headers <- character(0)
  repeat{
    ss <- readLines(con, n=1)
    if (verbose) {
      write(ss, stderr())
      flush(stderr())
    }
    if (ss == "") break
    key.value <- strsplit(ss, ":\\s*")
    response$headers[key.value[[1]][1]] <- key.value[[1]][2]
  }
  response$body = readLines(con)

  return(response$body)
}
