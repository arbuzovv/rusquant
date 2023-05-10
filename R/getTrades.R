#' @title Download trades of account
#'
#' @description Get trades for a given broker from a specified date to the current date.
#'
#' @param src Character string indicating the broker to use. Currently, only "tinkoff" and "alor" are supported.
#' @param api.key Character string containing the API key for the broker's API.
#' @param clientId Character string containing the client ID for the broker.
#' @param figi Character string containing the Financial Instrument Global Identifier for the broker.
#' @param from Date specifying the start date for the trade data. Defaults to 5 days ago.
#' @param to Date specifying the end date for the trade data. Defaults to the current date.
#' @param symbol_info Logical indicating whether to include symbol information in the returned data. Defaults to FALSE.
#' @param time_transform Logical indicating whether to transform the timestamps to the local timezone. Defaults to TRUE.
#' @param verbose Logical indicating whether to print verbose output. Defaults to FALSE.
#'
#' @return A data frame containing the trade data.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#'
#' @examples
#' getTrades(src = "tinkoff", api.key = "tks token", clientId = "clientID", figi = "figi",verbose=TRUE)
#' @export

getTrades = function(src='',api.key = '',clientId='',figi ='',from = Sys.Date()-5, to = Sys.Date(), symbol_info = FALSE, time_transform = TRUE, verbose = FALSE)
{
  src <- tolower(src)

  ## choose exchange
  if(src == 'tinkoff')
  {
    url <- "https://invest-public-api.tinkoff.ru/rest/"
    endpoint <- "tinkoff.public.invest.api.contract.v1.OperationsService/GetOperations"
    full_url <- paste0(url, endpoint)
    body <- list(accountId=clientId,
                 from=paste0(from,"T06:20:55.254Z"),
                 to=paste0(to,"T06:20:55.254Z"),
                 state="OPERATION_STATE_UNSPECIFIED",
                 figi=figi)
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
    jwt_token <- fromJSON(content(jwt_token, "text"))$AccessToken
    headers <- c("Authorization" = paste0("Bearer ", jwt_token))

    full_url = paste0('https://api.alor.ru/md/v2/clients/MOEX/',clientId,'/trades')
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
