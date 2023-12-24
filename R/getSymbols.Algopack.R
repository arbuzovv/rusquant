#' @title Download AlgoPack data from MOEX
#'
#' @description Download historical market data from AlgoPack  for a given symbol and time range.
#'
#' @param Symbols a character vector of AlgoPack symbols to download data for.
#' @param env environment where to create the downloaded data object.
#' @param from a character string indicating the start date of the data to download, in YYYY-MM-DD format.
#' @param to a character string indicating the end date of the data to download, in YYYY-MM-DD format.
#' @param verbose a logical indicating whether to print the response details or not.
#' @param type a character string indicating the AlgoPack type possible values are c('tradestats','orderstats','obstats'), default 'tradestats'.
#' @param login a character string with login from ISS Moex
#' @param password a character string with password from ISS Moex
#' @param auto.assign a logical indicating whether to automatically assign the downloaded data to the global environment.
#' @param ... additional arguments passed to getSymbols.AlgoPack
#' @return returns an data.table object containing financial data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Algopack('SBER',from = '2023-10-24',to='2023-11-04',login = 'my-email.com',password = 'mypassword')
#' getSymbols('SBER',src='Algopack',login = 'my-email.com',password = 'mypassword')
#' @export

getSymbols.Algopack <- function(Symbols,
                            env = globalenv(),
                            from=Sys.Date()-30,
                            to=Sys.Date(),
                            verbose=TRUE,
                            type = 'tradestats',
                            login = 'user@email.com',
                            password = 'mypassword',
                            auto.assign=FALSE,
                            ...)
{
  for(i in 1:length(Symbols))
  {
  Symbols.name = Symbols[i]
  algopack.downloadUrl <- paste0('https://iss.moex.com/iss/datashop/algopack/eq/',type,'/',tolower(Symbols.name),'.json')
  paginate = TRUE
  data_result = data.table()
  pagination_page = 0
  url_auth = 'https://passport.moex.com/authenticate'
  headers = c('Authorization' =  paste0('Basic ',base64encode(charToRaw(paste(login,password,sep = ':')))))
  auth_response <- GET(url_auth,add_headers(headers))
  if(auth_response$status_code!=200)
    return('authenticate to ISS Moex using login/password ')
  cookie_value = content(auth_response, "text", encoding = "UTF-8")
  headers = c('Cookie' =  paste0('MicexPassportCert=',cookie_value))

  while(paginate)
  {
    algopack.params = list('from'=from,
                           'till'=to,
                           start=pagination_page)
    response <- GET(algopack.downloadUrl, encode = "json",query = algopack.params,add_headers(headers))
    if(response$status_code==200)
    {
      json_response <- content(response, "text", encoding = "UTF-8")
      json_response <- fromJSON(json_response,simplifyMatrix = T)
      pagination_page = json_response$data.cursor$data[1,1]+ json_response$data.cursor$data[1,3]
      data_result = rbind(data_result,data.table(json_response$data$data))
      if(pagination_page >json_response$data.cursor$data[1,2])
        {
        paginate = FALSE
        if(nrow(data_result)!=0) setnames(data_result,json_response$data$columns)
        }
    }
    if(response$status_code!=200)
      if(verbose) return(content(response, as = "parsed"))
  }
  cols <- colnames(data_result)[-c(1:3,ncol(data_result))]
  data_result[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]
  if(auto.assign){
    assign(Symbols[[i]], data_result, env)
  }
  }
  if(auto.assign){
    return(Symbols)
  }
  return(data_result)
}


