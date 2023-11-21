#' @title Download data from Comon copytrading platform
#'
#' @description Download historical market data from Comon for a given trader id
#'
#' @param Symbols a character id of strategy on Comon
#' @param env environment where to create the downloaded data object.
#' @param period a character string indicating the frequency of the data to download. Possible values are 'day'.
#' @param verbose a logical indicating whether to print the response details or not.
#' @param auto.assign a logical indicating whether to automatically assign the downloaded data to the global environment.
#' @param ... additional arguments passed to getSymbols.Comon
#' @return returns an data.table object containing financial data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getSymbols.Comon('115038')
#' getSymbols('115039',src='Comon')
#' @export

getSymbols.Comon <- function(Symbols,
                            env = globalenv(),
                            period='day',
                            verbose=TRUE,
                            auto.assign=FALSE,
                            ...)
{
  headers = ''
  for(i in 1:length(Symbols))
  {
  Symbols.name = Symbols[i]
  data_result = fromJSON(paste0('https://www.comon.ru/api/v1/strategies/',Symbols.name,'/profit'))$data
  if(auto.assign){
    assign(Symbols[[i]], data_result, env)
  }
  }
  if(auto.assign){
    return(Symbols)
  }
  return(data_result)
}


