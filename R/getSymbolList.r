#' @title Get a list of symbols for a given stock exchange
#'
#' @description This function retrieves a list of symbols for a specified stock exchange from a variety of sources.
#' The available sources are poloniex, rusquant, tinkoff, mfd, finam, alor, and kraken.
#' The function returns a data.table object containing the symbol information for the requested exchange.
#
#' @param src character indicating the source of the symbol list.
#'   Possible values are "poloniex", "rusquant", "tinkoff", "mfd", "finam", "alor", and "kraken".
#' @param verbose logical indicating whether or not to print additional information.
#'   The default is FALSE.
#' @param auto.assign logical indicating whether or not to automatically assign the data.table object to the global environment.
#'   The default is FALSE.
#' @param country character indicating the country of the exchange. The default is an empty string.
#' @param api.key character indicating the API key to be used for accessing the source.
#'   The default is an empty string.
#' @param type character indicating the type of financial instruments to retrieve.
#'   Applicable for the "tinkoff" source and "gigapack". Possible values are "Bonds", "Currencies", "Etfs", "Futures", "Options", and "Shares".
#' @param env The environment where the data should be assigned. Defaults to the global environment.
#' @param user_agent The special headers for parsing
#'
#' @return Returns a data.table object containing the symbol information for the specified exchange.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbolList()
#' @export

getSymbolList <- function(src='poloniex',
                  verbose=FALSE,
                  auto.assign=FALSE,
                  country='',
                  api.key='',
                  type = 'Shares',
                  env = globalenv(),
                  user_agent = NULL)
{
  src <- tolower(src)

  if(src == 'moex')
  {
    data_moex = fromJSON('https://iss.moex.com/iss/engines/stock/markets/shares/boards/TQBR/securities.json')
    rawdata_m = data.table(data_moex$securities$data)
    names(rawdata_m) = data_moex$securities$columns
    result <-paste('symbol_list',toupper(gsub('\\^','',src)),sep='_')
    if(auto.assign)
    {
      assign(result, rawdata_m,env)
      return(result)
    }
    return(rawdata_m)
  }



  if(src == 'comon')
  {

    rawdata_m = fromJSON('https://www.comon.ru/api/v1/strategies?page=1&pageSize=30000')$data
    result <-paste('symbol_list',toupper(gsub('\\^','',src)),sep='_')
    if(auto.assign)
    {
      assign(result, rawdata_m,env)
      return(result)
    }
    return(rawdata_m)
  }


  if(src == 'gigapack')
  {
    rawdata_m = fromJSON('https://api.rusquant.io/gigafields')
    if(type == 'tech') rawdata_m = fromJSON('https://api.rusquant.io/gigafields?type=tech')
    if(type == 'candles') rawdata_m = fromJSON('https://api.rusquant.io/gigafields?type=candles')
    result <-paste('gigafields',toupper(gsub('\\^','',src)),sep='_')
    if(auto.assign)
    {
      assign(result, rawdata_m,env)
      return(result)
    }
    return(rawdata_m)
  }

  if(src == 'rusquant')
  {
    rusquant.url <- 'https://api.rusquant.io/performance'
    rusquant.params = list('token' = api.key,
                           'market' = 'ru')
    response <- GET(rusquant.url,query = rusquant.params)
    if(response$status_code==200)
    {
      rawdata_m <- data.table(fromJSON(content(response, "text", encoding = "UTF-8")))
      result <-paste('symbol_list',toupper(gsub('\\^','',src)),sep='_')
      if(auto.assign)
      {
        assign(result, rawdata_m,env)
        return(result)
      }
      return(rawdata_m)
    }
    if(response$status_code!=200)
      if(verbose) return(content(response,"text"))
  }


  ## choose exchange
  if(src == 'tinkoff')
  {
    type_list = c('Bonds','Currencies','Etfs','Futures','Options','Shares')
    url <- "https://invest-public-api.tinkoff.ru/rest/"
    endpoint <- "tinkoff.public.invest.api.contract.v1.InstrumentsService/"
    full_url <- paste0(url, endpoint,type)
    body <- list(instrumentStatus= "INSTRUMENT_STATUS_UNSPECIFIED")
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

  if(src == 'mfd')
  {
    for(i in 2:40)
    {
      marketId <- list(marketId = as.character(i))
      res <- POST("https://mfd.ru/export/getinstrumentoptions", body = marketId)
      datas_html =  (content(res,as = 'text'))
      datas_html  = gsub(pattern = '\\u0027',replacement = '',x = datas_html)
      datas_html2 = str_split(datas_html[[1]][1], "u003c")[[1]]
      datas_html2 = datas_html2[-1]
      datas_html2  = gsub(pattern = '[^[:alnum:]]+',replacement = '',x = datas_html2)
      datas_html2  = gsub(pattern = 'optiondatan',replacement = '',x = datas_html2)
      datas_html2  = gsub(pattern = 'u003e',replacement = 'value',x = datas_html2)
      datas_html2  = gsub(pattern = 'Successtrue',replacement = '',x = datas_html2)
      datas_html2  = gsub(pattern = 'value',replacement = ',',x = datas_html2)
      tmp <- tempfile()
      fwrite(x = data.table(datas_html2),tmp,quote = F,col.names = F,row.names = F)
      x = fread(tmp)
      unlink(tmp)
      x$marketId = marketId
      if(nrow(x)>3)
      {
        if(i==2) rawdata_m = x
        if(i!=2) rawdata_m = rbind(rawdata_m,x)
      }
    }
  }

  if(src=='finam' & api.key!='')
  {
    url = 'https://trade-api.finam.ru'
    endpoint = '/api/v1/securities/'
    full_url <- paste0(url, endpoint)
    headers = c('X-Api-Key' =  api.key)
    response <- GET(full_url, body = body, encode = "json", add_headers(headers))
    if(response$status_code==200)
    {
      json_response <- content(response, "text", encoding = "UTF-8")
      data_result <- fromJSON(json_response)$data$securities
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
    full_url = paste0('https://api.alor.ru/md/v2/Securities/','MOEX')
    response <- GET(full_url, encode = "json", add_headers(headers))


    if(response$status_code==200)
    {
      json_response<- content(response, "text", encoding = "UTF-8")
      data_result <- fromJSON(json_response)
      return(data_result)
    }
    if(response$status_code!=200)
      if(verbose) return(content(response, as = "parsed"))
  }
  ## choose exchange
  if(src == 'kraken')
    downloadUrl <- 'https://api.kraken.com/0/public/AssetPairs'

  if(src == 'poloniex')
    downloadUrl <- 'https://api.poloniex.com/markets'

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
  {
    if(is.null(user_agent)) user_agent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/112.0.0.0 Safari/537.37'
    headers <- c('User-Agent' = user_agent,
                 'Accept-Encoding' = 'gzip, deflate, br',
                 'Connection' = 'keep-alive',
                 'Accept' = '*/*',
                 'Accept-Language' =  'ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7',
                 'Cookie' = 'spid=1682942564505_7a797cb3cdda0503f908d14fd8a252b8_xacf4eg9lnilbpp1; spst=1682942564505_5fad132e5a44315687e98a808ee9f684_b350434491ba66d4f19a71c5c6917fb3; srv_id=d06601caa3d3302db5f7c901416c08c2')
    downloadUrl <- 'https://www.finam.ru/cache/N72Hgd54/icharts/icharts.js'
  }

  if(src == 'investing')
  {
    country_dict <- t(matrix(c("5","USA","29","Argentina","25","Australia","54","Austria","145","Bahrain","47","Bangladesh","34","Belgium","174","Bosnia","163","Botswana","32","Brazil","70","Bulgaria","6","Canada","27","Chile","37","China","122","Colombia","15","Costa_Rica","78","Cote_dIvoire","113","Croatia","107","Cyprus","55","Czech_Republic","24","Denmark","59","Egypt","72","Europe","71","Finland","22","France","17","Germany","51","Greece","39","Hong_Kong","93","Hungary","106","Iceland","14","India","48","Indonesia","66","Iraq","33","Ireland","23","Israel","10","Italy","119","Jamaica","35","Japan","92","Jordan","102","Kazakhstan","57","Kenya","94","Kuwait","68","Lebanon","103","Luxembourg","111","Malawi","42","Malaysia","109","Malta","188","Mauritius","7","Mexico","139","Mongolia","247","Montenegro","105","Morocco","172","Namibia","21","Netherlands","43","New_Zealand","20","Nigeria","60","Norway","87","Oman","44","Pakistan","193","Palestine","125","Peru","45","Philippines","53","Poland","38","Portugal","170","Qatar","100","Romania","56","Russian_Federation","80","Rwanda","52","Saudi_Arabia","238","Serbia","36","Singapore","90","Slovakia","112","Slovenia","110","South_Africa","11","South_Korea","26","Spain","162","Sri_Lanka","9","Sweden","12","Switzerland","46","Taiwan","85","Tanzania","41","Thailand","202","Tunisia","63","Turkey","123","Uganda","61","Ukraine","143","Dubai","4","UK","138","Venezuela","178","Vietnam","84","Zambia","75","Zimbabwe"),2,92))
    id_country <- country_dict[country_dict[,2] %in% country,1]
    downloadUrl <- paste0('http://www.investing.com/stock-screener/Service/downloadData?download=1&country%5B%5D=',id_country,'&sector=&industry=&equityType=&pn=')
  }
  ### download data ####

  if (!src %in% c("finam","investing",'mfd'))
    rawdata_m <- jsonlite::fromJSON(downloadUrl, simplifyVector = TRUE)
  if (src == "finam"  & api.key=='')
  {
    tmp <- tempfile()
    download.file(downloadUrl, destfile = tmp, mode = "wb",headers = headers)
  }

  if (src == "investing")
  {
    num_row <- 50
    i <- 1
    while(num_row==50)
    {
      finalUrl <- paste0(downloadUrl,i,'&order%5Bcol%5D=eq_market_cap&order%5Bdir%5D=d&tab=overview')
      rawdata <- fread(finalUrl,showProgress=FALSE)
      num_row <- nrow(rawdata)
      if(i == 1)
        rawdata_m <- rawdata
      if(i != 1)
        rawdata_m <- rbind(rawdata_m,rawdata)
      i=i+1
    }
  }

  # clean data
  if (src == "finam"  & api.key=='')
  {
    fr <- readLines(con = tmp, warn = FALSE,encoding = 'UTF-8')
    unlink(tmp)
    fr = iconv(fr, "WINDOWS-1251", "UTF8")
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
    rawdata_m$Id = suppressWarnings(as.numeric(rawdata_m$Id))
    rawdata_m$Market = as.numeric(rawdata_m$Market)
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
  if (src == "binance")
    names(rawdata_m) <- c('Symbol','Price')
  if (src %in% c("gate","gemini"))
  {
    rawdata_m <- data.table(rawdata_m)
    names(rawdata_m) <- 'Symbol'
  }
  if (src %in% c("gdax","hitbtc","lykke"))
    names(rawdata_m)[1]<- 'Symbol'


  result <-paste('symbol_list',toupper(gsub('\\^','',src)),sep='_')
  if(auto.assign)
  {
    assign(result, rawdata_m,env)
    return(result)
  }
  return(rawdata_m)
}



