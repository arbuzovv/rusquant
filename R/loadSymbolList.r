
 #function retrives All avalible instruments of exchange
 "loadSymbolList" <- function 
 (src='poloniex',
  verbose=FALSE,
  auto.assign=FALSE,
  country='',
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
   
   if(src == 'finam')
     downloadUrl <- 'https://finam.ru/cache/N72Hgd54/icharts/icharts.js'

   if(src == 'investing')
   {
     country_dict <- t(matrix(c("5","USA","29","Argentina","25","Australia","54","Austria","145","Bahrain","47","Bangladesh","34","Belgium","174","Bosnia","163","Botswana","32","Brazil","70","Bulgaria","6","Canada","27","Chile","37","China","122","Colombia","15","Costa_Rica","78","Cote_dIvoire","113","Croatia","107","Cyprus","55","Czech_Republic","24","Denmark","59","Egypt","72","Europe","71","Finland","22","France","17","Germany","51","Greece","39","Hong_Kong","93","Hungary","106","Iceland","14","India","48","Indonesia","66","Iraq","33","Ireland","23","Israel","10","Italy","119","Jamaica","35","Japan","92","Jordan","102","Kazakhstan","57","Kenya","94","Kuwait","68","Lebanon","103","Luxembourg","111","Malawi","42","Malaysia","109","Malta","188","Mauritius","7","Mexico","139","Mongolia","247","Montenegro","105","Morocco","172","Namibia","21","Netherlands","43","New_Zealand","20","Nigeria","60","Norway","87","Oman","44","Pakistan","193","Palestine","125","Peru","45","Philippines","53","Poland","38","Portugal","170","Qatar","100","Romania","56","Russian_Federation","80","Rwanda","52","Saudi_Arabia","238","Serbia","36","Singapore","90","Slovakia","112","Slovenia","110","South_Africa","11","South_Korea","26","Spain","162","Sri_Lanka","9","Sweden","12","Switzerland","46","Taiwan","85","Tanzania","41","Thailand","202","Tunisia","63","Turkey","123","Uganda","61","Ukraine","143","Dubai","4","UK","138","Venezuela","178","Vietnam","84","Zambia","75","Zimbabwe"),2,92))
     id_country <- country_dict[country_dict[,2] %in% country,1]
     downloadUrl <- paste0('https://www.investing.com/stock-screener/Service/downloadData?download=1&country%5B%5D=',id_country,'&sector=&industry=&equityType=&pn=')
   }
   ### download data ####

   if (!src %in% c("finam","investing"))
     rawdata_m <- jsonlite::fromJSON(downloadUrl, simplifyVector = TRUE)		
   if (src == "finam")	
   {
     tmp <- tempfile()
     download.file(downloadUrl, destfile = tmp,method = "libcurl")
   }
   
   if (src == "investing")	
   {
     num_row <- 50
     i <- 1 
     while(num_row==50)
     { 
       finalUrl <- paste0(downloadUrl,i,'&order%5Bcol%5D=eq_market_cap&order%5Bdir%5D=d&tab=overview')
       rawdata <- read.csv(finalUrl)
       num_row <- nrow(rawdata)
       if(i == 1)
         rawdata_m <- rawdata
       if(i != 1)
         rawdata_m <- rbind(rawdata_m,rawdata)
       i=i+1
      }
   }
   
   # clean data
   if (src == "finam")	
   {
     fr <- readLines(con = tmp, warn = FALSE)
     unlink(tmp)
     EmitentIds <- sub("var .*?= \\[", "", fr[1])
     EmitentIds <- sub("\\];", "", EmitentIds)
     EmitentIds <- strsplit(EmitentIds, ",")
     EmitentNames <- sub("var .*?= \\[", "", fr[2])
     EmitentNames <- sub("\\];", "", EmitentNames)
     EmitentNames <- strsplit(EmitentNames, "','")
     EmitentCodes <- sub("var .*?= \\[", "", fr[3])
     EmitentCodes <- sub("\\];", "", EmitentCodes)
     EmitentCodes <- strsplit(EmitentCodes, "','")
     EmitentMarkets <- sub("var .*?= \\[", "", fr[4])
     EmitentMarkets <- sub("\\];", "", EmitentMarkets)
     EmitentMarkets <- strsplit(EmitentMarkets, ",")
     EmitentUrls <- sub("var .*?= \\{", "", fr[9])
     EmitentUrls <- sub("\\];", "", EmitentUrls)
     EmitentUrls <- strsplit(EmitentUrls, ",")
     
     rawdata_m <- data.table(as.character(EmitentCodes[[1]]),as.character(EmitentNames[[1]]),as.character(EmitentIds[[1]]),as.character(EmitentMarkets[[1]]),as.character(EmitentUrls[[1]]))
     names(rawdata_m) <- c('Symbol','Name','Id','Market','Url')
     rm(list = c('EmitentIds','EmitentCodes','EmitentNames','EmitentMarkets','EmitentUrls','fr'))
   }
   
   if (src == "liqui")	
   {
     rawdata_m <- t(sapply(rawdata_m$pairs,cbind))
     rawdata_m <- data.table(rownames(rawdata_m),rawdata_m)
     names(rawdata_m)[1] <- 'Symbol'
   }
   if (src == "gatecoin")		
   {
     rawdata_m <- rawdata_m$tickers
     names(rawdata_m)[1] <- 'Symbol'
   }
   if (src == "cex")
   {
     rawdata_m <- rawdata_m$data$pairs
     rawdata_m$Symbol <- paste0(rawdata_m[,1], "/", rawdata_m[,2])
   }
   if (src == "bttrex")
   {
     rawdata_m <- rawdata_m$result	
     names(rawdata_m)[6] <- 'Symbol'
   }
   if (src == "kraken")	
   {
     rawdata_m <- data.table(t(sapply(rawdata_m$result,function(x) t(data.frame(x[c(1:9,14:15)])))))
     names(rawdata_m)[1] <- 'Symbol'
   }
   if (src == "poloniex")	
   {
     colnms <- names(rawdata_m[[1]])
     rownms <- names(rawdata_m)
     rawdata_m <- (t(sapply(rawdata_m,cbind)))
     rawdata_m <- data.table(rownms,rawdata_m)
     names(rawdata_m) <- c('Symbol',colnms)
   }
   if (src == "binance")
     names(rawdata_m) <- c('Symbol','Price')
   if (src %in% c("gate","gemini"))	
   {
     rawdata_m <- data.table(rawdata_m)
     names(rawdata_m) <- 'Symbol'
   }
   if (src %in% c("gdax","hitbtc","lykke"))		
     names(rawdata_m)[1]<- 'Symbol'
   
   
   
   result <-paste('symbol_list_',toupper(gsub('\\^','',src)),sep='_')			
   if(auto.assign)                
   {
     assign(result, rawdata_m,globalenv())			
     return(result)
   }										    
   return(rawdata_m)
 }