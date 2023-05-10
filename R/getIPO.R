#' @title Get IPO calendar data from Investing.com
#'
#' @description This function retrieves IPO calendar data from Investing.com for a specified date range
#'
#' @param from The start date of the date range in YYYY-MM-DD format. Default is the current date minus 10 days.
#' @param to The end date of the date range in YYYY-MM-DD format. Default is the current date.
#' @return A data frame containing IPO calendar data for the specified date range.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getIPO(from=Sys.Date(),to=Sys.Date()+3)
#' @export


"getIPO" <- function(from=Sys.Date()-10,to=Sys.Date())
{
  url = 'https://www.investing.com/ipo-calendar/Service/getCalendarFilteredData'
  end_date = to
  date = from

  data = paste0('country%5B%5D=29&country%5B%5D=25&country%5B%5D=54&country%5B%5D=145&country%5B%5D=34&country%5B%5D=174&country%5B%5D=163&country%5B%5D=32&country%5B%5D=70&country%5B%5D=6&country%5B%5D=27&country%5B%5D=37&country%5B%5D=122&country%5B%5D=15&country%5B%5D=113&country%5B%5D=107&country%5B%5D=55&country%5B%5D=24&country%5B%5D=59&country%5B%5D=71&country%5B%5D=22&country%5B%5D=17&country%5B%5D=51&country%5B%5D=39&country%5B%5D=93&country%5B%5D=106&country%5B%5D=14&country%5B%5D=48&country%5B%5D=33&country%5B%5D=23&country%5B%5D=10&country%5B%5D=35&country%5B%5D=92&country%5B%5D=57&country%5B%5D=94&country%5B%5D=68&country%5B%5D=103&country%5B%5D=42&country%5B%5D=109&country%5B%5D=188&country%5B%5D=7&country%5B%5D=105&country%5B%5D=172&country%5B%5D=21&country%5B%5D=43&country%5B%5D=20&country%5B%5D=60&country%5B%5D=87&country%5B%5D=44&country%5B%5D=193&country%5B%5D=125&country%5B%5D=45&country%5B%5D=53&country%5B%5D=38&country%5B%5D=170&country%5B%5D=100&country%5B%5D=56&country%5B%5D=52&country%5B%5D=238&country%5B%5D=36&country%5B%5D=90&country%5B%5D=112&country%5B%5D=110&country%5B%5D=11&country%5B%5D=26&country%5B%5D=162&country%5B%5D=9&country%5B%5D=12&country%5B%5D=46&country%5B%5D=41&country%5B%5D=202&country%5B%5D=63&country%5B%5D=123&country%5B%5D=61&country%5B%5D=143&country%5B%5D=4&country%5B%5D=5&country%5B%5D=138&country%5B%5D=178&country%5B%5D=75&',
                'dateFrom=',date,'&dateTo=',end_date,'&currentTab=custom&submitFilters=1&limit_from=0')

  headers = add_headers('Host' = 'www.investing.com',
                        'Origin' = 'https://www.investing.com',
                        'Referer' = 'https://www.investing.com/earnings-calendar/',
                        'X-Requested-With' = 'XMLHttpRequest',
                        'Content-Type' = 'application/x-www-form-urlencoded',
                        'Connection' = 'keep-alive',
                        'Accept-Language' = 'en-US,en;q=0.9,fr;q=0.8,ja;q=0.7,es;q=0.6',
                        'Accept-Encoding' = 'Encoding:gzip, deflate',
                        'Accept' = '*/*',
                        'User-Agent'= 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/63.0.3239.108 Safari/537.36')

  r <- POST(url,headers,body = data,encode = "raw")

  if(status_code(r) == 200)
  {
    cr <- content(r,as = 'text')
    json_data <- fromJSON(cr)
    json_data2 <- gsub('\n','',json_data$data)
    json_data2 <- gsub('\t','',json_data2)
    json_data2 <- gsub('\"','',json_data2)

    ### each row
    html_rvest <- read_html(json_data2)
    symbols <- html_nodes(html_rvest, "tr")
    for(i in 1:length(symbols))
    {
      if(length(html_nodes(symbols[i], "span"))>0)
      {
        country <- as.character(html_nodes(symbols[i], "span")[1])
        country <- gsub('\"','',country)
        symbol_name <- as.character(html_nodes(symbols[i], "td")[2])
        date  <- as.character(html_nodes(symbols[i], "td")[1])
        exchange  <- as.character(html_nodes(symbols[i], "td")[3])
        ipo_value  <- as.character(html_nodes(symbols[i], "td")[4])
        ipo_price  <- as.character(html_nodes(symbols[i], "td")[5])
        last_price  <- as.character(html_nodes(symbols[i], "td")[6])
        symbol_ticker <- as.character(html_nodes(symbols[i], "a"))
        country <- str_match(country, "title=([^ ]+)")[,2]
        symbol_name <- strsplit(symbol_name, '>|<')[[1]][9]
        date <- strsplit(date, '>|<')[[1]][3]
        date <- as.Date(date,'%b %d, %Y')
        exchange <- strsplit(exchange, '>|<')[[1]][3]
        ipo_value <- strsplit(ipo_value, '>|<')[[1]][3]
        ipo_price <- strsplit(ipo_price, '>|<')[[1]][3]
        last_price <- strsplit(last_price, '>|<')[[1]][3]
        symbol_ticker <- ifelse(length(symbol_ticker)>0,strsplit(symbol_ticker, '>|<')[[1]][3],NA)
        record = data.table(country=country,
                            name=symbol_name,
                            symbol=symbol_ticker,
                            date = date,
                            exchange = exchange,
                            ipo_value= ipo_value,
                            ipo_price = ipo_price,
                            last_price = last_price)
        if(i==1)
          Records <- record
        if(i!=1)
          Records <- rbind(Records,record)
      }
    }
  }
  if(status_code(r) != 200)
  {
    Records <- 'cannot connect to server'
  }
  return(Records)
}





