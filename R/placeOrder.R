#' @title Place an order on a broker/exchange platform
#'
#' @description This function place an existing order on a specified broker or exchange platform
#'
#' @param src A character string indicating the broker/exchange. Currently supported sources are 'tinkoff', 'finam', and 'alor'.
#' @param symbol A character string indicating the security symbol.
#' @param board A character string indicating the exchange board.
#' @param action A character string indicating the action to perform. Possible values are 'BUY' and 'SELL'.
#' @param orderType A character string indicating the type of order. Possible values are 'LMT' (limit) and 'MKT' (market).
#' @param totalQuantity A character string or numeric value indicating the quantity of securities to order.
#' @param lmtPrice A character string or numeric value indicating the limit price for the order.
#' @param auxPrice A character string or numeric value indicating the auxiliary price for the order. This parameter is only used when orderType is 'STP' (stop).
#' @param api.key A character string indicating the API key to use for authentication with the broker/exchange.
#' @param live A logical value indicating whether to place a live (real) order or a test (paper) order.
#' @param tif A character string indicating the time-in-force of the order. Possible values are 'DAY', 'GTC' (good till cancel), and 'IOC' (immediate or cancel).
#' @param orderId A character string indicating the order ID to use. If empty, a random ID is generated.
#' @param clientId A character string indicating the client ID to use.
#' @param verbose A logical value indicating whether to print verbose output.
#'
#' @return A list with the result of the order placement.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @examples
#' myorder = placeOrder(src = 'alor',
#'                      symbol = 'MTLR-6.23',
#'                      board = 'MOEX',
#'                      action = 'BUY',
#'                      orderType = 'LMT',
#'                      totalQuantity = 1,
#'                      lmtPrice = 20000,
#'                      api.key = '',
#'                      clientId = 'cliendID')
#' @import quantmod
#' @import data.table
#' @import jsonlite
#' @import httr
#' @import XML
#' @import stringr
#' @import utils
#' @import rvest
#' @import base64enc
#' @importFrom jose jwt_split
#' @importFrom stats runif
#' @importFrom xts xts
#'
#' @export

placeOrder = function(src = 'tinkoff',
                      symbol = 'SBER',
                      board = 'MOEX',
                      action='BUY',
                      orderType = 'LMT',
                      totalQuantity='10',
                      lmtPrice = '100',
                      auxPrice='',
                      api.key = '',
                      live = TRUE,
                      tif = "",
                      orderId = '',
                      clientId = '',
                      verbose=TRUE)
{
  ## choose broker/exchange
  if(src == 'tinkoff')
  {
    url <- "https://invest-public-api.tinkoff.ru/rest/"
    endpoint <- "tinkoff.public.invest.api.contract.v1.OrdersService/PostOrder"
    full_url <- paste0(url, endpoint)
    body <- list(figi = symbol,
                 quantity = totalQuantity,
                 price = list(units = lmtPrice,nano = 0),
                 direction = action,
                 accountId=clientId,
                 orderType = orderType,
                 orderId = '3',
                 instrumentId = '1')

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
    headers = c('X-Api-Key' =  api.key)
    body <- list("clientId"=clientId,
                   "securityBoard"=board,
                   "securityCode"=symbol,
                   "buySell"=action,
                   "quantity"=totalQuantity,
                   "useCredit"=TRUE,
                   "price"=lmtPrice,
                   "property"="PutInQueue")
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
    if(orderId == '') orderId = as.character(round(runif(1,1,1000^3)))
    order_type = ifelse(orderType == 'LMT','limit','market')
    if(api.key!='')
    {
    jwt_token = POST(paste0('https://oauth.alor.ru/refresh?token=',api.key))
    if(jwt_token$status_code==200)
    {
    jwt_token <- fromJSON(content(jwt_token, "text"))$AccessTokens
    headers <- c("Authorization" = paste0("Bearer ", jwt_token),
                 "X-ALOR-REQID" = as.character(orderId))
    body <- list('side' = tolower(action),
                 'type' = order_type,
                 'quantity' = totalQuantity,
                 'price' = lmtPrice,
                 'instrument' = list(
                 'symbol'=symbol,
                 'exchange'=board),
                 'user' = list(portfolio=clientId),
                 'timeInForce'='OneDay')

    full_url = paste0('https://api.alor.ru/commandapi/warptrans/TRADE/v2/client/orders/actions/',order_type)
    response <- POST(full_url,body = body, encode = "json", add_headers(headers))

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
  }
}


