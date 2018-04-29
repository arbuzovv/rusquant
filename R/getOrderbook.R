
#function retrives Tradelog data from Poloniex through its public API
"getOrderbook" <- function #S3 function (Poloniex is a class of first argument)
(Symbols,return.class='data.table',index.class='Date',
 depth=10,
 src='poloniex',
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
		src <- tolower(src)
       
	  ## choose exchange       
	  if(src == 'kraken')
		downloadUrl <- paste0("https://api.kraken.com/0/public/Depth?pair=", Symbols)
	  if(src == 'poloniex')		
		downloadUrl <- paste0("https://poloniex.com/public?command=returnOrderBook&currencyPair=",Symbols,"&depth=", depth)
	  if (src == "binance")
		downloadUrl <- url <- paste0("https://api.binance.com/api/v1/depth?symbol=",Symbols)
#	  if (src == "bitfinex")
#		downloadUrl <- paste0("https://api.bitfinex.com/v1/book/", tolower(Symbols))
#	  if (src == "bitstamp")
#		downloadUrl <- paste0("https://www.bitstamp.net/api/v2/order_book/",tolower(asset_pair))
	  if (src == "bttrex")
		downloadUrl <- paste0("https://bittrex.com/api/v1.1/public/getorderbook?market=",Symbols,"&type=both")
	  if (src == "cex")
		downloadUrl <- paste0("https://cex.io/api/order_book/",Symbols[1], "/", Symbols[2])		
	  if (src == "gate")
		downloadUrl <- paste0("http://data.gate.io/api2/1/orderBook/",Symbols, "")
	  if (src == "gatecoin")
		downloadUrl <- paste0("https://api.gatecoin.com/Public/MarketDepth/", Symbols)
	  if (src == "gdax")
		downloadUrl <- paste0("https://api.gdax.com/products/",Symbols,"/book?level=2")		
	  if(src == "gemini")
		downloadUrl <- paste0("https://api.gemini.com/v1/book/", Symbols)		
	  if (src == "hitbtc")
		downloadUrl <- paste0("https://api.hitbtc.com/api/2/public/orderbook/", Symbols)
	  if (src == "liqui")
		downloadUrl <- paste0("https://api.liqui.io/api/3/depth/",Symbols,"?limit=", depth)
	  if (src == "lykke") 
		downloadUrl <- paste0("https://hft-api.lykke.com/api/OrderBooks/", Symbols)
	  if (src == "xbtce") 
		downloadUrl <- paste0("https://cryptottlivewebapi.xbtce.net:8443/api/v1/public/level2/",Symbols)

rawdata <- jsonlite::fromJSON(downloadUrl, simplifyVector = FALSE)				  
				  
if (src == "kraken")	
	{
	ask <- t(sapply(rawdata$result[[1]]$asks,rbind))
	bid <- t(sapply(rawdata$result[[1]]$bids,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)
	orderbook <- orderbook[,-3]
	}				  
if (src == "binance")	
	{
	ask <- t(sapply(rawdata$asks,rbind))
	bid <- t(sapply(rawdata$bids,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)
	orderbook <- orderbook[,-3]
	}	
if (src == "bttrex")	
	{		
	ask <- t(sapply(rawdata$result$sell,rbind))
	bid <- t(sapply(rawdata$result$buy,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)
	orderbook <- orderbook[,c(2,1,3)]
	}
if (src %in% c("gatecoin","gate","cex","gdax","poloniex","gemini"))
	{		
	ask <- t(sapply(rawdata$asks,rbind))
	bid <- t(sapply(rawdata$bids,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)
	}		
if (src %in% c("gdax","gemini"))
	{		
	ask <- t(sapply(rawdata$asks,rbind))
	bid <- t(sapply(rawdata$bids,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)
	orderbook <- orderbook[,-3]
	}		
	
if (src == "hitbtc")
	{		
	ask <- t(sapply(rawdata$ask,rbind))
	bid <- t(sapply(rawdata$bid,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)
	}	
if (src == "liqui")	
	{			
	ask <- t(sapply(rawdata[[1]]$asks,rbind))
	bid <- t(sapply(rawdata[[1]]$bids,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)      
	}
if (src == "lykke")	
	{			
	ask <- t(sapply(rawdata[[1]]$Prices,rbind))
	bid <- t(sapply(rawdata[[2]]$Prices,rbind))
	ask <- data.table(ask,1)
	bid <- data.table(bid,0)
	orderbook <- rbind(ask,bid)   
	orderbook <- orderbook[,c(2,1,3)]
	}
if (src == "xbtce")	
	{		
	ask <- t(sapply(rawdata[[1]]$Asks,rbind))
	bid <- t(sapply(rawdata[[1]]$Bids,rbind))
	ask <- data.table(ask[,1:2],1)
	bid <- data.table(bid[,1:2],0)
	orderbook <- rbind(ask,bid)  
	orderbook <- orderbook[,c(2,1,3)]
	}
						  			 
	names(orderbook)[1:3] <- c('Price','Volume','isAsk')			
	orderbook[,Price:=as.numeric(Price),]
	orderbook[,Volume:=as.numeric(Volume),]
	orderbook[,isAsk:=as.numeric(isAsk),]
	orderbook <- orderbook[order(Price,decreasing = TRUE)]
				
    Symbols[1] <-paste('Order_Book_',toupper(gsub('\\^','',Symbols[1])),sep='_')
        if(auto.assign){
            assign(Symbols[1], orderbook,globalenv())
                }
        
        if(auto.assign){
                return(Symbols)
        }
        
        return(orderbook)
}