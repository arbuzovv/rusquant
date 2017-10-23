
#function retrives Tradelog data from Poloniex through its public API
"getOrderbook" <- function #S3 function (Poloniex is a class of first argument)
(Symbols,return.class='data.table',index.class='Date',
 depth=10,
 adjust=FALSE,
 verbose=FALSE,
 auto.assign=TRUE,
 ...)
{
        importDefaults("getOrderbook"); #rewrite default values if specified by setDefaults
        env <- environment()
        for(var in names(list(...))) {
                # import all named elements that are NON formals
                assign(var, list(...)[[var]], local_env)
        }
        
        options(warn = -1)
        default.return.class <- return.class
        if(missing(verbose)) verbose <- TRUE
       
        Polo.downloadUrl <- 'https://poloniex.com/public?command=returnOrderBook'
        
        #example API usage
        # https://poloniex.com/public?command=returnOrderBook&currencyPair=BTC_NXT&depth=10
        
        for(i in 1:length(Symbols)) {
                Polo.url <- paste(Polo.downloadUrl,
                                  "&currencyPair=", Symbols[[i]],
                                  "&depth=", depth, sep="")
                tmp <- tempfile()
				if(verbose == TRUE) print(paste('Downloading file from',Polo.url))
                download.file(Polo.url, destfile = tmp, quiet = TRUE) #get JSON object
				date_time <- Sys.time()
                rawdata <- readLines(tmp) #read raw data from file
                if(substr(rawdata, 3, 7) == 'error') {
                        stop(paste('Error!', substr(rawdata, 11, nchar(rawdata) - 2)))
                }
                
                #parsing JSON object
				str_obs <- strsplit(rawdata, split = ":", fixed = TRUE)
				ask <- strsplit(str_obs[[1]][2], split = "],[", fixed = TRUE)
				bid <- strsplit(str_obs[[1]][3], split = "],[", fixed = TRUE)
				frozen_flag <- strsplit(str_obs[[1]][4], split = ",", fixed = TRUE)			
				ask <- gsub('"','',ask[[1]])
				ask <- gsub(']],bids','',ask)
				ask <- gsub('[[]','',ask)
				bid <- gsub('"','',bid[[1]])
				bid <- gsub(']],isFrozen','',bid)
				bid <- gsub('[[]','',bid)
				frozen_flag <- frozen_flag[[1]][1]
				frozen_flag <- gsub('"','',frozen_flag[1])				
				seq_orderbook  <- gsub('}','',str_obs[[1]][5])		
				ask <- t(data.frame(strsplit(ask, split = ",", fixed = TRUE)))
				bid <- t(data.frame(strsplit(bid, split = ",", fixed = TRUE)))
				ask <- data.table(ask,1)
				bid <- data.table(bid,0)
				orderbook <- rbind(ask,bid)
				orderbook$DateTime <- date_time
				orderbook$Seq <- seq_orderbook
				names(orderbook)[1:3] <- c('Price','Volume','isAsk')
				orderbook <- orderbook[order(Price,decreasing = TRUE)]
				
                fr <- convert.time.series(fr = orderbook, return.class=return.class)                
                Symbols[1] <-paste('Order_Book_',toupper(gsub('\\^','',Symbols[1])),sep='_')
                if(auto.assign){
                        assign(Symbols[1], fr,globalenv())
                }
        }
        
        if(auto.assign){
                return(Symbols)
        }
        
        return(fr)
}