#' @title Download AlgoPack data from MOEX
#'
#' @description Download historical market data from AlgoPack  for a given symbol and time range.
#'
#' @param Symbols a character vector of AlgoPack symbols to download data for.
#' @param env environment where to create the downloaded data object.
#' @param from a character string indicating the start date of the data to download, in YYYY-MM-DD format.
#' @param to a character string indicating the end date of the data to download, in YYYY-MM-DD format.
#' @param date a character string indicating the date of the data to download, in YYYY-MM-DD format.
#' @param verbose a logical indicating whether to print the response details or not.
#' @param type a character string indicating the AlgoPack type possible values are c('tradestats','orderstats','obstats','hi2','oi'), default 'tradestats'.
#' @param market a character string indicating the market type possible values are c('eq','fo','fx'), default 'eq'.
#' @param auto.assign a logical indicating whether to automatically assign the downloaded data to the global environment.
#' @param ... additional arguments passed to getSymbols.AlgoPack
#' @return returns an data.table object containing financial data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Algopack('SBER',from = '2023-10-24',to='2023-11-04')
#' # open interest for available futures
#' getSymbols.Algopack(date = '2024-05-10',type = 'oi')
#' # open interest for Si futures
#' getSymbols.Algopack(Symbols = 'Si',type = 'oi')
#' # market concentration for available stocks
#' #getSymbols.Algopack(date = '2024-05-10',type = 'hi2')
#' # market concentration for current stock
#' #devgetSymbols.Algopack('SBER',from = '2023-10-24',to='2023-11-04',type = 'hi2')
#' # market concentration for available fx
#' #getSymbols.Algopack(date = '2024-05-10',type = 'hi2',market='fx')
#' # market concentration for available futures
#' #getSymbols.Algopack(date = '2024-05-10',type = 'hi2',market='fo')
#' # market concentration for CNYRUB_TOM
#' #getSymbols.Algopack(Symbols = 'CNYRUB_TOM',type = 'hi2',market='fx')
#' @export

getSymbols.Algopack <- function(Symbols='',
                            env = globalenv(),
                            from=Sys.Date()-30,
                            to=Sys.Date(),
                            date=Sys.Date(),
                            verbose=FALSE,
                            type = 'tradestats',
                            market = 'eq',
                            auto.assign=FALSE,
                            ...)
{
  login <- Sys.getenv('MOEX_DATASHOP_LOGIN')
  password <- Sys.getenv('MOEX_DATASHOP_PASSWORD')
  cookie_value <- Sys.getenv('MOEX_DATASHOP_COOKIE')
  if(login == '' & password=='')
    return('authenticate to ISS Moex using login/password ')
  for(i in 1:length(Symbols))
  {
  Symbols.name = Symbols[i]
  algopack.downloadUrl <- paste0('https://iss.moex.com/iss/datashop/algopack/',market,'/',type,'/',tolower(Symbols.name),'.json')
  if(Symbols.name=='')
    algopack.downloadUrl <- paste0('https://iss.moex.com/iss/datashop/algopack/',market,'/',type,'.json')
  if(type == 'oi')
  {
    algopack.downloadUrl <- paste0('https://iss.moex.com/iss/analyticalproducts/futoi/securities.json')
    if(Symbols.name!='')
      algopack.downloadUrl <- paste0('https://iss.moex.com/iss/analyticalproducts/futoi/securities/',tolower(Symbols.name),'.json')
  }

  paginate = TRUE
  data_result = data.table()
  pagination_page = 0
  headers = c('Cookie' =  paste0('MicexPassportCert=',cookie_value))
  while(paginate)
  {
    algopack.params = list('from'=from,
                           'till'=to,
                           start=pagination_page)
    if(Symbols.name=='')
      algopack.params = list('date'=date,
                             start=pagination_page)
    tryCatch(
      {
        response <- GET(algopack.downloadUrl, encode = "json",query = algopack.params,add_headers(headers))
        if(verbose==T) print(response$url)
        if(response$status_code==200)
        {
          json_response <- content(response, "text", encoding = "UTF-8")
          json_response <- fromJSON(json_response,simplifyMatrix = T)
          pagination_page = json_response$data.cursor$data[1,1]+ json_response$data.cursor$data[1,3]
          data_result = rbind(data_result,data.table(json_response$data$data))
          if(type == 'oi')
          {
            data_result = rbind(data_result,data.table(json_response$futoi$data))
            setnames(data_result,json_response$futoi$columns)
            paginate = FALSE
          }


          if(pagination_page >json_response$data.cursor$data[1,2])
          {
            paginate = FALSE
            if(nrow(data_result)!=0) setnames(data_result,json_response$data$columns)
          }
        }
        if(response$status_code!=200)
          if(verbose) return(content(response, as = "parsed"))
      },
      #if an error occurs, tell me the error
      error=function(e) {
        message('Server of MOEX not response - try later')
        #print(e)
      },
      #if a warning occurs, tell me the warning
      warning=function(w) {
        message('Check your internet connection')
      }
    )
  }
  if(type == 'hi2')
    data_result$value = as.numeric(data_result$value)
  if(type != 'hi2' & type != 'oi')
    {
      cols <- colnames(data_result)[-c(1:3,ncol(data_result))]
      data_result[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    }
  if(auto.assign){
    assign(Symbols[[i]], data_result, env)
  }
  }
  if(auto.assign){
    return(Symbols)
  }
  return(data_result)
}


