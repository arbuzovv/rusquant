#' @title Retrieve Orders Information from Brokers
#'
#' @description This function retrieves information about orders from different brokers/exchanges
#'
#' @param src Character string specifying the source broker/exchange (e.g., "tinkoff", "finam", "alor")
#' @param board Character string specifying the trading board (default is "MOEX")
#' @param api.key Character string specifying the API key for the broker/exchange
#' @param orderId Character string specifying the order ID to retrieve (default is "")
#' @param clientId Character string specifying the client ID for the broker/exchange
#' @param stopOrders Logical specifying whether to retrieve stop orders (default is FALSE)
#' @param verbose Logical specifying whether to display additional information (default is TRUE)
#'
#' @return A list containing order information from the broker/exchange
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @seealso \code{\link{getTrades}} \code{\link{cancelOrder}} \code{\link{placeOrder}}
#' @examples
#' # Retrieve all orders from Tinkoff
#' getOrders(src = "tinkoff", api.key = "your_api_key", clientId = "your_client_id")
#'
#' # Retrieve all orders from Finam
#' getOrders(src = "finam", api.key = "your_api_key", clientId = "your_client_id")
#'
#' # Retrieve all orders from Alor
#' getOrders(src = "alor", api.key = "your_api_key", clientId = "your_client_id")
#' @export

getOrders = function(src = '',
                     board = 'MOEX',
                     api.key = '',
                     orderId = '',
                     clientId = '',
                     stopOrders = FALSE,
                     verbose = TRUE)
  {
  src <- tolower(src)

  ## choose broker/exchange
  if(src == 'tinkoff')
  {
    url <- "https://invest-public-api.tinkoff.ru/rest/"
    endpoint <- "tinkoff.public.invest.api.contract.v1.OrdersService/GetOrders"
    full_url <- paste0(url, endpoint)
    body <- list(accountId=clientId)
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

  if(src=='finam')
  {
    url = 'https://trade-api.finam.ru'
    endpoint = ifelse(stopOrders,'/api/v1/stops/','/api/v1/orders/')
    params <- list(clientId=clientId,
                   IncludeExecuted = 'true',
                   IncludeMatched = 'true',
                   IncludeCanceled = 'true',
                   IncludeActive = 'true')
    full_url <- paste0(url,endpoint, "?", paste(names(params),params, sep='=',collapse = "&"))
    headers = c('X-Api-Key' =  api.key)
    response <- GET(full_url, encode = "json", add_headers(headers))
    if(response$status_code==200)
    {
      json_response <- content(response, "text", encoding = "UTF-8")
      data_result <- fromJSON(json_response)$data
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

    full_url = paste0('https://api.alor.ru/md/v2/clients/',board,'/',clientId,'/orders')
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
}
