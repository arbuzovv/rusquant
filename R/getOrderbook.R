#' @title Get  the order book for a given symbol from a supported exchange
#'
#' @description This function retrieves order book information
#'
#' @param Symbols A character vector specifying the symbol to retrieve order book data for.
#' @param depth An integer value specifying the number of levels of the order book to retrieve. Defaults to 10.
#' @param src A character string specifying the exchange to retrieve data from. Possible values are  kraken,poloniex,tinkoff,alor, binance.
#' @param adjust A logical value indicating whether to adjust timestamps to match the system timezone. Defaults to FALSE.
#' @param verbose A logical value indicating whether to print detailed messages during the function's execution. Defaults to FALSE.
#' @param auto.assign A logical value indicating whether to automatically assign the resulting object to the current environment. Defaults to TRUE.
#' @param api.key A character string specifying the API key to use when retrieving data from Alor or Tinkoff. Defaults to "".
#' @param env environment where the data will be assigned
#'
#' @return A data.table object containing the order book data for the specified symbol.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#'
#' @examples
#' getOrderbook('USDTGBP', src = 'kraken')
#' getOrderbook('BTC_USDT', src = 'poloniex')
#' @export

"getOrderbook" <- function #S3 function (Poloniex is a class of first argument)
(Symbols,
 depth=10,
 src='poloniex',
 adjust=FALSE,
 verbose=FALSE,
 auto.assign=TRUE,
 api.key = '',
 env=globalenv())
{

		src <- tolower(src)
		Price <- Volume <- isAsk <- NULL
	  ## choose exchange
		if(src == 'tinkoff')
		{
		  url <- "https://invest-public-api.tinkoff.ru/rest/"
		  endpoint <- "tinkoff.public.invest.api.contract.v1.MarketDataService/GetOrderBook"
		  full_url <- paste0(url, endpoint)
		  body <- list(figi= Symbols,
		               depth= "10",
		               instrumentId= "INSTRUMENT_STATUS_UNSPECIFIED")
		  headers <- c("Authorization" = paste("Bearer", api.key))
		  response <- POST(full_url, body = body, encode = "json", add_headers(headers))

		  if(response$status_code==200)
		  {
		    json_response <- content(response, "text", encoding = "UTF-8")
		    data_result <- fromJSON(json_response)
		    return(data_result)
		  }
		  if(response$status_code!=200)
		    if(verbose) return(content(response, as = "parsed"))
		}
		if(src == 'alor')
		{
		  jwt_token = POST(paste0('https://oauth.alor.ru/refresh?token=',api.key))
		  jwt_token <- fromJSON(content(jwt_token, "text"))$AccessToken
		  headers <- c("Authorization" = paste0("Bearer ", jwt_token))
		  full_url = paste0('https://api.alor.ru/md/v2/orderbooks/MOEX/',Symbols)
		  response <- GET(full_url, encode = "json", add_headers(headers))

		  if(response$status_code==200)
		  {
		    json_response <- content(response, "text", encoding = "UTF-8")
		    data_result <- fromJSON(json_response)
		    return(data_result)
		  }
		  if(response$status_code!=200)
		    if(verbose) return(content(response, as = "parsed"))
		}

	  if(src == 'kraken')
		  downloadUrl <- paste0("https://api.kraken.com/0/public/Depth?pair=", Symbols)
	  if(src == 'poloniex')
	    downloadUrl <- paste0('https://api.poloniex.com/markets/',Symbols,'/orderBook?scale=1&limit=',depth)
	  if (src == "binance")
		  downloadUrl <- url <- paste0("https://api.binance.com/api/v1/depth?symbol=",Symbols)
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
            assign(Symbols[1], orderbook,env)
                }

        if(auto.assign){
                return(Symbols)
        }

        return(orderbook)
}
