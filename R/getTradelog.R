#' @title Get tradelog information from a brokerage or exchange
#'
#' @description This function retrieves account information from a brokerage or exchange.
#'
#' @param Symbols Character vector specifying the trading pair, e.g. "BTC_ETH".
#' @param depth Numeric scalar, specifying the number of trades to retrieve (default = 500).
#' @param src Character scalar, specifying the exchange to retrieve trade logs from. The supported exchanges are: "tinkoff", "alor", "poloniex", "kraken", "binance", "bttrex", "cex", "gate", "gatecoin", "gdax", "gemini", "hitbtc", "liqui", and "lykke".
#' @param api.key Character scalar, specifying the API key (if required by the exchange).
#' @param adjust Logical scalar, specifying whether to adjust timestamps for time zones (default = FALSE).
#' @param return.class Character scalar, specifying the class of the returned object. The supported classes are: "data.table", "data.frame", and "xts" (default = "data.table").
#' @param index.class Character scalar, specifying the class of the index column. The supported classes are: "Date" and "POSIXct" (default = "Date").
#' @param verbose Logical scalar, specifying whether to print verbose output (default = FALSE).
#' @param auto.assign Logical scalar, specifying whether to automatically assign the resulting object to the global environment (default = TRUE).
#' @param env environment where data is stored
#'
#' @return A data table or data frame with the retrieved trade logs, depending on the value of the \code{return.class} argument.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getTradelog('BTC_USDT', src = 'poloniex')
#' @export

"getTradelog" <- function
(Symbols,depth=500,src='poloniex',api.key = '',
 adjust=FALSE,return.class='data.table',index.class='Date',
 verbose=FALSE,
 auto.assign=TRUE,env=globalenv())
{
        src <- tolower(src)
        ## choose exchange
        if (src == "tinkoff")
        {
          url <- "https://invest-public-api.tinkoff.ru/rest/"
          endpoint <- "tinkoff.public.invest.api.contract.v1.MarketDataService/GetLastTrades"
          full_url <- paste0(url, endpoint)
          body <- list(figi= Symbols,
                       from="2023-04-10T06:01:20.553Z",
                       to="2023-04-12T06:01:20.553Z",
                       instrumentId="string")
          headers <- c("Authorization" = paste("Bearer", api.key))
          response <- POST(full_url, body = body, encode = "json", add_headers(headers))

          if(response$status_code==200)
          {
            json_response <- content(response, "text", encoding = "UTF-8")
            data_result <- fromJSON(json_response)[[1]]
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
          full_url = paste0('https://api.alor.ru/md/v2/Securities/MOEX/SBER/alltrades')
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
        if (src == "poloniex")
                downloadUrl <- paste0('https://api.poloniex.com/markets/',Symbols,'/trades?limit=',depth)
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
                trades <- data.frame(trades[,c(1,5,3,2,4)])
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind),format= "%Y-%m-%dT%H:%M:%S",origin="1970-01-01", tz ="GMT")
        }
        if (src == "gemini")
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('timestamp','timestampms','tid', 'price', 'amount', 'exchange', 'type')
                trades <- data.frame(trades[,c(3,1,5,4,7)])
                trades[,2] <- as.POSIXct(as.numeric(sapply(trades[,2],rbind)),origin="1970-01-01", tz ="GMT")
        }
        if (src == "gdax")
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('time', 'trade_id', 'price', 'size', 'side')
                trades <- data.frame(trades[,c(2,1,4,3,5)])
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind),format= "%Y-%m-%dT%H:%M:%S",origin="1970-01-01", tz ="GMT")
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
                trades <- data.frame(trades[,c(5,2,3,4,1)])
                trades[,2] <- as.POSIXct(as.numeric(sapply(trades[,2],rbind)),origin="1970-01-01", tz ="GMT")
        }
        if (src == "bttrex")
        {
                trades <- t(sapply(rawdata$result,rbind))
                colnames(trades) <- c('Id','TimeStamp', 'Quantity', 'Price', 'Total', 'FillType', 'OrderType')
                trades <- data.frame(trades[,c(1:4,7)])
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind),format= "%Y-%m-%dT%H:%M:%S", tz ="GMT")
        }
        if (src == "gatecoin")
        {
                trades <- t(sapply(rawdata$transactions,cbind))
                colnames(trades) <- c('transactionId', 'transactionTime', 'price', 'quantity')
                trades <- data.frame(trades[,c(1,2,4,3)])
                trades$Type<-''
                trades[,2] <- as.POSIXct(as.numeric(sapply(trades[,2],rbind)),origin="1970-01-01", tz ="GMT")
        }
        if (src == "liqui")
        {
                trades <- t(sapply(rawdata[[1]],rbind))
                colnames(trades) <- c('type', 'price', 'amount', 'tid', 'timestamp')
                trades <- data.frame(trades[,c(4,5,3,2,1)])
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind),origin="1970-01-01", tz ="GMT")
                trades[,5] <- ifelse(trades[,5]=='bid','buy','sell')
        }
        if (src == "lykke")
        {
                trades <- (t(sapply(rawdata,rbind)))
                colnames(trades) <- c('Id', 'assetPairId', 'DateTime', 'Volume', 'TradeType','Price','Type')
                trades <- data.frame(trades[,c(1,3,4,6,7)])
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind),format= "%Y-%m-%dT%H:%M:%S", tz ="GMT")
        }
        if (src == "binance")
        {
                trades <- t(sapply(rawdata,rbind))
                colnames(trades) <- c('id', 'price', 'qty', 'time', 'isBuyerMaker', 'isBestMatch')
                trades <- data.frame(trades[,c(1,4,3,2,5)])
                trades[,5] <- ifelse(trades[,5]==TRUE,'buy','sell')
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind)/1000, origin="1970-01-01", tz ="GMT") # kraken


        }
        if (src == "kraken")
        {
                trades <- t(sapply(rawdata$result[[1]],rbind))
                colnames(trades) <- c('price', 'volume', 'time', 'buy/sell', 'market/limit', 'miscellaneous')
                trades <- data.frame('',trades[,c(3,2,1,4)])
                trades[,5] <- ifelse(trades[,5]=='b','buy','sell')
                trades[,2] <- as.POSIXct(sapply(trades[,2],rbind), origin="1970-01-01", tz ="GMT")
        }
        if (src == "poloniex")
        {
                trades <- t(sapply(rawdata,rbind))
                #colnames(trades) <- c('Id','tradeID','DateTime','Type','Price','Volume','total')
                #trades <- data.frame(trades[,c(1,3,6,5,4)])
                #trades[,2] <- as.POSIXct(sapply(trades[,2],rbind), origin="1970-01-01", tz ="GMT")
        }

        trades <- data.table(trades)

        Symbols[1] <-paste0('TradeLog_',toupper(gsub('\\^','',Symbols[1])))
        if(auto.assign){
                assign(Symbols[1], trades,env)
                return(Symbols)
        }

        return(trades)
}
