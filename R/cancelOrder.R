#' @title Cancel an order on a broker/exchange platform
#'
#' @description This function cancels an existing order on a specified broker or exchange platform
#'
#' @param src character string, the name of the broker/exchange platform, e.g. "tinkoff", "finam", "alor"
#' @param api.key character string, the API key required for authentication
#' @param orderId character string, the ID of the order to be cancelled
#' @param clientId character string, the ID of the client account
#' @param board character string, the name of the exchange board, required for some platforms
#' @param live logical, whether to execute the order in a live environment, default is TRUE
#' @param verbose logical, whether to print the HTTP response message, default is TRUE
#' @return character string, the response message from the HTTP request
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @examples
#' cancelOrder(src = 'Finam',api.key = 'finam_token',orderId = 'otderID',clientId = 'your cliend id')
#' @export

cancelOrder = function(src = '',api.key = '',orderId = '',clientId = '',board = '', live = TRUE,verbose=TRUE)
{
  ## choose broker/exchange
  if(src == 'tinkoff')
  {
    url <- "https://invest-public-api.tinkoff.ru/rest/"
    endpoint <- "tinkoff.public.invest.api.contract.v1.OrdersService/CancelOrder"
    full_url <- paste0(url, endpoint)
    body <- list(accountId=clientId,
                 orderId = orderId)
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

  if(src == 'finam')
  {
    url = 'https://trade-api.finam.ru'
    endpoint = '/api/v1/orders/'
    full_url <- paste0(url, endpoint)

    body = list(
      clientId = clientId,
      transactionId = as.integer(orderId))


    headers = c('X-Api-Key' =  api.key)
    response <- DELETE(full_url, query = body, add_headers(headers))
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

    full_url = paste0('https://api.alor.ru/commandapi/warptrans/TRADE/v2/client/orders/',orderId,'?portfolio=',clientId,'&exchange=',board)
    response <- DELETE(full_url, encode = "json", add_headers(headers))

    if(response$status_code==200)
    {
      data_result <- content(response, "text", encoding = "UTF-8")
      return(data_result)
    }
    if(response$status_code!=200)
      if(verbose) return(content(response, as = "parsed"))
  }
}


