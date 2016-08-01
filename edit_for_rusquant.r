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

