#' @title Download Giga Candles data from GigaPack
#'
#' @description Download historical market data from GigaPack  for a given symbol and time range.
#'
#' @param Symbols a character vector of AlgoPack symbols to download data for.
#' @param env environment where to create the downloaded data object.
#' @param from a character string indicating the start date of the data to download, in YYYY-MM-DD format.
#' @param to a character string indicating the end date of the data to download, in YYYY-MM-DD format.
#' @param date a character string indicating the specific date of the data to download, in YYYY-MM-DD format.
#' @param verbose a logical indicating whether to print the response details or not.
#' @param field a character string indicating the GigaPack field
#' @param type a character string indicating the GigaPack field candles or tech
#' @param fake a bool string indicating the Giga Candles or Alter Giga
#' @param reps number of alternative history geneartion
#' @param trim number of entropy in data
#' @param auto.assign a logical indicating whether to automatically assign the downloaded data to the global environment.
#' @param ... additional arguments passed to getSymbols.AlgoPack
#' @return returns an data.table object containing financial data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Gigapack('SBER', field = 'disb.q20')
#' @export

getSymbols.Gigapack <- function(Symbols,
                            env = globalenv(),
                            from=Sys.Date()-30,
                            to=Sys.Date(),
                            date = '',
                            verbose=TRUE,
                            field = '',
                            type = 'candles',
                            fake = FALSE,
                            reps = 1,
                            trim = 0.1,
                            auto.assign=FALSE,
                            ...)
{
  for(i in 1:length(Symbols))
  {
  Symbols.name = Symbols[i]
  if(type == 'candles')
  {
    if(fake == FALSE)
    {
      if(field == '') field = 'close'
      gigapack.downloadUrl  = paste0('https://api.rusquant.io/gigacandles?symbol=',Symbols.name,'&field=',field,'&orient=table&date=',date)
      data_result  = data.table(fromJSON(gigapack.downloadUrl))
      data_result = data_result[order(date)]
    }
    if(fake == TRUE)
    {
      if(field == '') field = 'close'
      gigapack.downloadUrl  = paste0('https://api.rusquant.io/altergiga?symbol=',Symbols.name,'&field=',field,'&trim=',trim,'&reps=',reps,'&orient=table')
      data_result  = data.table(fromJSON(gigapack.downloadUrl))
      data_result = data_result[order(date)]
    }
  }
  if(type == 'tech')
  {
    field_q=''
    if(field!='') field_q = paste0('&field=',field)
    if(fake == FALSE)
    {
      gigapack.downloadUrl  = paste0('https://api.rusquant.io/gigatech?symbol=',Symbols.name,'&orient=table',field_q)
      data_result  = data.table(fromJSON(gigapack.downloadUrl))
      data_result = data_result[order(date)]
    }
    if(fake == TRUE)
    {
      gigapack.downloadUrl  = paste0('https://api.rusquant.io/altertech?symbol=',Symbols.name,'&orient=table',field_q,'&trim=',trim,'&reps=',reps)
      data_result  = data.table(fromJSON(gigapack.downloadUrl))
      data_result = data_result[order(date)]
    }
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


