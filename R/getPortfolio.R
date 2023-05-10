#' @title Retrieve portfolio data from different brokers/exchanges
#' @description This function retrieves portfolio data from different brokers/exchanges such as Tinkoff, Finam and Alor.
#' @param src character indicating the name of the broker/exchange (options are 'tinkoff', 'finam', or 'alor')
#' @param board character indicating the name of the board (only required for Alor, default is 'MOEX')
#' @param api.key character representing the authorization key required for accessing broker/exchange API
#' @param clientId character representing the ID of the client whose portfolio data is being retrieved
#' @param verbose logical value indicating whether to print verbose output (default is TRUE)
#' @return A list of portfolio data containing the positions and other relevant information
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' # Retrieve portfolio data from Tinkoff
#' getPortfolio(src = 'tinkoff', api.key = 'my_api_key', clientId = 'my_client_id')
#'
#' # Retrieve portfolio data from Finam
#' getPortfolio(src = 'finam', api.key = 'my_api_key', clientId = 'my_client_id')
#'
#' # Retrieve portfolio data from Alor
#' getPortfolio(src = 'alor', api.key = 'my_api_key', clientId = 'my_client_id', board = 'MOEX')
#' @export

getPortfolio = function(src = '',
                        board = 'MOEX',
                        api.key = '',
                        clientId = '',
                        verbose=TRUE)
{
  src <- tolower(src)

  ## choose BROKER/EXCHANGE
  if(src == 'tinkoff')
  {
    url <- "https://invest-public-api.tinkoff.ru/rest/"
    endpoint <- "tinkoff.public.invest.api.contract.v1.OperationsService/GetPortfolio"
    full_url <- paste0(url, endpoint)
    body <- list(accountId=clientId,
                currency="RUB")
    headers <- c("Authorization" = paste("Bearer", api.key))
    response <- POST(full_url, body = body, encode = "json", add_headers(headers))

    if(response$status_code==200)
    {
      json_response <- content(response, "text", encoding = "UTF-8")
      data_result <- fromJSON(json_response)
      return(data_result)
    }
    if(response$status_code!=200)
      if(verbose) return(content(response, as = "parsed"))
  }
  if(src=='finam')
  {
    url = 'https://trade-api.finam.ru'
    endpoint = '/api/v1/portfolio/'
    params <- list(clientId=clientId,
                 Content.IncludeCurrencies = 'true',
                 Content.IncludeMoney = 'true',
                 Content.IncludePositions = 'true',
                 Content.IncludeMaxBuySell = 'true')
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
    full_url = paste0('https://api.alor.ru/md/v2/clients/',board,'/',clientId,'/summary')
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
