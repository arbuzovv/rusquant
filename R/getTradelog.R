
#function retrives Tradelog data from Poloniex through its public API
"getTradelog" <- function #S3 function (Poloniex is a class of first argument)
(Symbols,env,return.class='xts',index.class='Date',
 from=Sys.Date(),
 to=Sys.Date(),
 adjust=FALSE,
 verbose=FALSE)
{
        importDefaults("getTradelog"); #rewrite default values if specified by setDefaults
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
        
        Polo.period <- 0
        Polo.max_candles <- 1000
        Polo.from <- as.numeric(as.POSIXct(from)) #convert to UNIX timestamp
        Polo.to <- as.numeric(as.POSIXct(to)) #convert to UNIX timestamp
        Polo.downloadUrl <- 'https://poloniex.com/public?command=returnTradeHistory'

        
        #example API usage
        # https://poloniex.com/public?command=returnTradeHistory&currencyPair=BTC_NXT&start=1410158341&end=1410499372
        
        for(i in 1:length(Symbols)) {
                Polo.url <- paste(Polo.downloadUrl,
                                  "&currencyPair=", Symbols[i],
                                  "&start=", Polo.from,
                                  "&end=", Polo.to, sep="")
                tmp <- tempfile()
				if(verbose) print(Polo.url)
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
                
                ticker_header <- c('globalTradeID','tradeID','date','type','rate','amount','total')
                str_obs <- data.frame(str_obs[[1]])
                res <- apply(str_obs,1,function(x) gsub('"','',strsplit(x, ",")[[1]]))
                res <- t(res)
                for(i in 1:length(ticker_header))
                        res[,i] <- gsub(paste(ticker_header[i],':',sep = ''),'',res[,i])

                date_time <- fast_strptime(res[,3],format = "%Y-%m-%d %H:%M:%S")
                res[,4] <- ifelse(res[,4]=='buy',1,0)
                res <- res[,-3]
                res <- apply(res[,1:6],2,as.numeric)
                res <- xts(res[,c(1:2,4:6,3)],order.by = date_time)

                names(res) <- c('globalTradeID','TradeID','Price','Volume','Value','BuySell')
                fr <- convert.time.series(fr = res, return.class=return.class)
                
                Symbols[i] <-toupper(gsub('\\^','',Symbols[i]))
                
                if(auto.assign){
                        assign(Symbols[i], fr, env)
                }
        }
        
        if(auto.assign){
                return(Symbols)
        }
        
        return(fr)
}