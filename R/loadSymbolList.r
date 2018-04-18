


#function retrives All avalible instruments of exchange
"loadSymbolList" <- function #S3 function (Poloniex is a class of first argument)
(src='poloniex',
 verbose=FALSE,
 auto.assign=FALSE,
 ...)
{
        importDefaults("loadSymbolList"); #rewrite default values if specified by setDefaults
        env <- environment()
		src <- tolower(src)
  
  ## choose exchange       
  if(src == 'kraken')
	downloadUrl <- 'https://api.kraken.com/0/public/AssetPairs'

  if(src == 'poloniex')		
	downloadUrl <- 'https://poloniex.com/public?command=returnTicker'
						
  if (src == "binance")
    downloadUrl <- "https://api.binance.com/api/v1/ticker/allPrices"

  if (src == "bitfinex")
    downloadUrl <- "https://api.bitfinex.com/v1/symbols"

  if (src == "bitflyer")
    downloadUrl <- "https://api.bitflyer.com/v1/getmarkets/usa"

  if (src == "bitstamp")
    downloadUrl <- "https://www.bitstamp.net/api/v2/trading-pairs-info/"

  if (src == "bttrex")
    downloadUrl <- "https://bittrex.com/api/v1.1/public/getmarkets"

  if (src == "cex")
    downloadUrl <- "https://cex.io/api/currency_limits"

  if (src == "gate")
    downloadUrl <- "http://data.gate.io/api2/1/pairs"

  if (src == "gatecoin")
    downloadUrl <- "https://api.gatecoin.com/Public/LiveTickers"

  if (src == "gdax")
    downloadUrl <- "https://api.gdax.com/products/"

  if(src == "gemini")
    downloadUrl <- "https://api.gemini.com/v1/symbols"
	
  if (src == "hitbtc")
    downloadUrl <- "https://api.hitbtc.com/api/2/public/symbol"

  if (src == "liqui")
    downloadUrl <- "https://api.liqui.io/api/3/info"

  if (src == "lykke") 
    downloadUrl <- "https://hft-api.lykke.com/api/AssetPairs"

  if (src == "xbtce") 
    downloadUrl <- "https://cryptottlivewebapi.xbtce.net:8443/api/v1/public/symbol"

#download and parse data			
rawdata_m <- jsonlite::fromJSON(downloadUrl, simplifyVector = TRUE)			
			
result <-paste('symbol_list_',toupper(gsub('\\^','',src)),sep='_')			
if(auto.assign)                
	{
    assign(result, rawdata_m,globalenv())			
	return(result)
	}										    
return(rawdata_m)
}