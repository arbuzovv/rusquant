

library(httr)
library(jsonlite)
library(rvest)
library(XML)

"getInvesting_id" <- function(Symbol='AAPL')
{
  url = 'https://www.investing.com/search/service/search'
  
  headers = add_headers('Host' = 'www.investing.com',
                        'Origin' = 'https://www.investing.com',
                        'Referer' = 'https://www.investing.com/search/service/search',
                        'X-Requested-With' = 'XMLHttpRequest',
                        'Content-Type' = 'application/x-www-form-urlencoded',
                        'Connection' = 'keep-alive',
                        'Accept-Language' = 'en-US,en;q=0.9,fr;q=0.8,ja;q=0.7,es;q=0.6',
                        'Accept-Encoding' = 'Encoding:gzip, deflate, br',
                        'Accept' = '*/*',
                        'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.108 Safari/537.36')
  
  data = paste0('search_text=',Symbol,'&term=',Symbol,'&country_id=0&tab_id=All')
  
  r <- POST(url,headers,body = data,encode = "raw")
  
  if(status_code(r) == 200)
  {
    cr <- content(r,as = 'text')
    json_data <- fromJSON(cr)
  }
  inv_id <- json_data$All$pair_ID[which.max(json_data$All$popularity_rank)]
  return(inv_id)
}







"getInvesting" <- function(from=Sys.Date()-10,to=Sys.Date(),Symbol='AAPL')
{
  url = 'https://www.investing.com/instruments/HistoricalDataAjax'
  # end_date = '2019-10-17'
  # start_date = '2019-01-02'
  start_date = from
  end_date = to
 
  headers = add_headers('Host' = 'www.investing.com',
                          'Origin' = 'https://www.investing.com',
                          'Referer' = 'https://www.investing.com/dividends-calendar/',
                          'X-Requested-With' = 'XMLHttpRequest',
                          'Content-Type' = 'application/x-www-form-urlencoded',
                          'Connection' = 'keep-alive',
                          'Accept-Language' = 'en-US,en;q=0.9,fr;q=0.8,ja;q=0.7,es;q=0.6',
                          'Accept-Encoding' = 'Encoding:gzip, deflate, br',
                          'Accept' = '*/*',
                          'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.108 Safari/537.36')
    
    data <- paste0(data = 'curr_id=',getInvesting_id(Symbol),'&header=',Symbol,'+Historical+Data&st_date=',format(as.Date(start_date),'%m/%d/%Y'),'&end_date=',format(as.Date(end_date),'%m/%d/%Y'),'&interval_sec=Daily&sort_col=date&sort_ord=DESC&action=historical_data')
    
    r <- POST(url,headers,body = data,encode = "raw")
    if(status_code(r) == 200)
    {
      cr <- content(r,as = 'text')
      cr <- gsub(',','',cr)
      Records <- readHTMLTable(cr)$curr_table
      if(ncol(Records)>2)
      {
      names(Records)[1:5] <- c('Date','Close','Open','High','Low')
      Records <- Records[,c(1,3,4,5,2,6)]
      Records$Date <- as.Date(as.character(Records$Date),'%b %d %Y')
      Records$Open <- as.numeric(as.character(Records$Open))
      Records$High <- as.numeric(as.character(Records$High))
      Records$Low <- as.numeric(as.character(Records$Low))
      Records$Close <- as.numeric(as.character(Records$Close))
      Records <- Records[order(Records$Date),]
      }
    }
    #print(Records)
    if(status_code(r) != 200)
    {
      Records <- 'cannot connect to server'
    }
  return(Records)
}

#x <- getDividends(country = 'China')
#price <- getInvesting(from = '2019-01-01',to='2019-10-10',Symbol = '002859')




