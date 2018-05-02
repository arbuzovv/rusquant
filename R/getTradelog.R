

"getTradelog" <- function 
(Symbols,depth=500,src='poloniex',
 adjust=FALSE,
 verbose=FALSE,
 auto.assign=TRUE,
 ...)       
{
        importDefaults("getTradelog"); #rewrite default values if specified by setDefaults
        env <- environment()
        for(var in names(list(...))) {
                # import all named elements that are NON formals
                assign(var, list(...)[[var]], local_env)
        }
        
        options(warn = -1)
        default.return.class <- return.class
        default.from <- from
        default.to <- to
        if(missing(verbose)) verbose <- TRUE
        src <- tolower(src)
        
        ## choose exchange       
        if (src == "poloniex")
                downloadUrl <- paste0("https://poloniex.com/public?command=returnTradeHistory&currencyPair=",Symbols)
        if(src == "kraken")
                downloadUrl <- paste0("https://api.kraken.com/0/public/Trades?pair=", Symbols)
        if (src == "binance")
                downloadUrl <- url <- paste0("https://api.binance.com/api/v1/trades?symbol=",Symbols) 
        if (src == "bttrex")
                downloadUrl <- paste0("https://bittrex.com/api/v1.1/public/getmarkethistory?market=",Symbols)
        if (src == "cex")
                downloadUrl <- paste0("https://cex.io/api/trade_history/",Symbols)
        if (src == "gate") #!!!!!!!!!!
                downloadUrl <- paste0("http://data.gate.io/api2/1/tradeHistory/",Symbols, "")
        if (src == "gatecoin")
                downloadUrl <- paste0("https://api.gatecoin.com/Public/TransactionsHistory/", Symbols,"?Count=",depth)
        if (src == "gdax")
                downloadUrl <- paste0("https://api.gdax.com/products/",Symbols,"/trades")		
        if(src == "gemini")
                downloadUrl <- paste0("https://api.gemini.com/v1/trades/", Symbols)
        if (src == "hitbtc")
                downloadUrl <- paste0("https://api.hitbtc.com/api/2/public/trades/", Symbols)
        if (src == "liqui")
                downloadUrl <- paste0("https://api.liqui.io/api/3/trades/",Symbols,"?limit=", depth)
        if (src == "lykke") 
                downloadUrl <- paste0("https://public-api.lykke.com/api/Trades/", Symbols,'?skip=0&take=',depth)

        rawdata <- jsonlite::fromJSON(downloadUrl, simplifyVector = FALSE)

        if (src == "hitbtc")
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('id', 'price', 'quantity', 'side', 'timestamp')
        }
        if (src == "gemini")
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('timestamp','timestampms','tid', 'price', 'amount', 'exchange', 'type')
                trades <- trades[,c(3,1,5,4,7)]
        }
        if (src == "gdax")
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('time', 'trade_id', 'price', 'size', 'side')
                trades <- trades[,c(2,1,4,3,5)]
        }
        if (src == "gate")
        {
                trades <- t(sapply(rawdata$data,rbind))
                colnames(trades) <- c('tradeID','date', 'timestamp', 'type', 'rate', 'amount', 'total')
        }
        if (src == "cex")	
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('type', 'date', 'amount', 'price', 'tid')
                trades <- trades[,c(5,2,3,4,1)]
        }
        if (src == "bttrex")	
        {
                trades <- t(sapply(rawdata$result,rbind))
                colnames(trades) <- c('Id','TimeStamp', 'Quantity', 'Price', 'Total', 'FillType', 'OrderType')
                trades <- trades[,c(1:4,7)]
        }
        if (src == "gatecoin")	
        {
                trades <- data.frame(t(sapply(rawdata$transactions,cbind)))
                colnames(trades) <- c('transactionId', 'transactionTime', 'price', 'quantity')
                trades$Type<-''
        }     
        if (src == "liqui")
        {
                trades <- t(sapply(rawdata[[1]],rbind))
                colnames(trades) <- c('type', 'price', 'amount', 'tid', 'timestamp')
        }				  
        if (src == "lykke")
        {
                trades <- (t(sapply(rawdata,rbind)))
                colnames(trades) <- c('Id', 'assetPairId', 'DateTime', 'Volume', 'Price', 'Type')
                trades <- trades[,c(1,3,4,5,6)]
        }	
        if (src == "binance")	
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('id', 'price', 'qty', 'time', 'isBuyerMaker', 'isBestMatch')
                trades <- trades[,c(1,4,3,2,5)]
                trades[,5] <- ifelse(trades[,5]==TRUE,'buy','sell')
        }
        if (src == "kraken")	
        {
                trades <- t(sapply(rawdata$result[[1]],rbind))
                colnames(trades) <- c('price', 'volume', 'time', 'buy/sell', 'market/limit', 'miscellaneous')
                trades <- data.frame('',trades[,c(3,2,1,4)])
                trades[,5] <- ifelse(trades[,5]=='b','buy','sell')
        }
        if (src == "poloniex")	
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('Id','tradeID','DateTime','Type','Price','Volume','total')
                trades <- trades[,c(1,3,6,5,4)]
        }
        
        trades <- data.table(trades)
        names(trades) <- c('Id','DateTime','Volume','Price','Type')
        trades$Type <- tolower(trades$Type)
        trades$Volume <- as.numeric(trades$Volume)
        trades$Price <- as.numeric(trades$Price)
     
        # Symbols[1] <-paste0('TradeLog_',toupper(gsub('\\^','',Symbols[1])))
        # if(auto.assign){
        #         assign(Symbols[1], trades,globalenv())
        #         return(Symbols)
        # }
        
       # return(trades)
}