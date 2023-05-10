#' @title Get account information from a brokerage or exchange
#'
#' @description This function retrieves account information from a brokerage or exchange.
#'
#' @param src a character string specifying the brokerage or exchange. Can be one of "tinkoff" or "alor". Default is "tinkoff".
#' @param api.key a character string representing the authorization key for the API.
#' @param verbose a logical value indicating whether to print detailed information about the request/response. Default is FALSE.
#'
#' @return A list object with account information, or an error message if the request fails.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#'
#' @examples
#' # get account information from tinkoff
#' account_info <- getAccounts(src = "Tinkoff", api.key = "your_api_key")
#'
#' # get account information from alor
#' account_info <- getAccounts(src = "Alor", api.key = "your_api_key")
#'
#' @export

getAccounts = function(src='tinkoff',api.key = '', verbose = FALSE)
{
  src <- tolower(src)
  if(src == 'tinkoff')
  {
  url <- "https://invest-public-api.tinkoff.ru/rest/"
  endpoint <- "tinkoff.public.invest.api.contract.v1.UsersService/GetAccounts"
  full_url <- paste0(url, endpoint)
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
  if(src == 'alor')
  {
    jwt_token = POST(paste0('https://oauth.alor.ru/refresh?token=',api.key))
    if(jwt_token$status_code==200)
    {
      jwt_token <- fromJSON(content(jwt_token, "text"))$AccessToken
      jwt_info = jwt_split(jwt_token)
      data_result = strsplit(jwt_info$payload$portfolios,' ')[[1]]
      return(data_result)
    }
    if(jwt_token$status_code!=200)
      if(verbose) return(content(jwt_token, as = "parsed"))
  }
}



