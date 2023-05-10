#' @title Download earnings data from investing.com
#'
#' @description This function retrieves earnings data from the investing.com website for a specified time period
#'
#' @param from the start date in yyyy-mm-dd format (default is 10 days prior to current date)
#' @param to the end date in yyyy-mm-dd format (default is the current date)
#' @param country the country from which to get earnings data (default is United States)
#' @return a data frame with earnings data
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getEarnings(from = Sys.Date(),to = Sys.Date()+3,country='Belgium')
#' @export

getEarnings <- function(from=Sys.Date()-5,to=Sys.Date()+5,country='United States')
{
  url = 'https://www.investing.com/earnings-calendar/Service/getCalendarFilteredData'
  #end_date = '2019-05-17'
  #start_date = '2019-05-01'
  end_date = to
  start_date = from
  country_dict <- t(matrix(c(29,'Argentina',25,'Australia',54,'Austria',145,'Bahrain',34,'Belgium',174,'Bosnia-Herzegovina',163,'Botswana',32,'Brazil',70,'Bulgaria',6,'Canada',27,'Chile',37,'China',122,'Colombia',15,'Costa Rica',113,'Croatia',107,'Cyprus',55,'Czech Republic',24,'Denmark',59,'Egypt',71,'Finland',22,'France',17,'Germany',51,'Greece',39,'Hong Kong',93,'Hungary',106,'Iceland',14,'India',48,'Indonesia',33,'Ireland',23,'Israel',10,'Italy',35,'Japan',92,'Jordan',57,'Kenya',94,'Kuwait',68,'Lebanon',103,'Luxembourg',42,'Malaysia',109,'Malta',188,'Mauritius',7,'Mexico',105,'Morocco',172,'Namibia',21,'Netherlands',43,'New Zealand',20,'Nigeria',60,'Norway',87,'Oman',44,'Pakistan',193,'Palestinian Territory',125,'Peru',45,'Philippines',53,'Poland',38,'Portugal',170,'Qatar',100,'Romania',56,'Russia',52,'Saudi Arabia',238,'Serbia',36,'Singapore',90,'Slovakia',112,'Slovenia',110,'South Africa',11,'South Korea',26,'Spain',162,'Sri Lanka',9,'Sweden',12,'Switzerland',46,'Taiwan',41,'Thailand',202,'Tunisia',63,'Turkey',123,'Uganda',61,'Ukraine',143,'United Arab Emirates',4,'United Kingdom',5,'United States',138,'Venezuela',178,'Vietnam',75,'Zimbabwe'),2,80))

  if(country == '')
    id_country <- country_dict[,1]

  if(country != '')
  {
    id_country <- country_dict[country_dict[,2] %in% country,1]
    if(length(id_country) == 0)
    {
      id_country <- country_dict[,1]
    }
  }

  date_diff <- as.Date(end_date)-as.Date(start_date)
  date_range <- as.Date(start_date)+1:date_diff

  pb <- txtProgressBar(min = 1, max = as.numeric(date_diff), style = 3) # Создаём progress bar


  for(date in date_range)
  {
    setTxtProgressBar(pb, as.numeric(as.Date(date,origin = "1970-01-01")-as.Date(start_date,origin = "1970-01-01"))) # Обновляем progress bar
    data = paste0(paste0('country%5B%5D=',id_country,'&',collapse = ''),
                  'dateFrom=',as.Date(date,origin = "1970-01-01"),'&dateTo=',as.Date(date,origin = "1970-01-01"),'&currentTab=custom&submitFilters=1&limit_from=0')
    if(length(date_range)==1)
      data = paste0(paste0('country%5B%5D=',id_country,'&',collapse = ''),
                    'dateFrom=',start_date,'&dateTo=',end_date,'&currentTab=custom&submitFilters=1&limit_from=0')


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
    current_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    for(i in 1:length(symbols))
    {
      if(length(html_nodes(symbols[i], "span"))==0)
      {
        date_node <- as.character(html_nodes(symbols[i], "td"))
        date  <- strsplit(date_node, '>|<')[[1]][3]
        date <- as.Date(date,'%a, %b %d, %Y')
      }
      if(length(html_nodes(symbols[i], "span"))>0)
      {
        country <- as.character(html_nodes(symbols[i], "span")[1])
        country <- gsub('\"','',country)
        symbol_name <- as.character(html_nodes(symbols[i], "td")[2])
        eps_act <- as.character(html_nodes(symbols[i], "td")[3])
        eps_for <- as.character(html_nodes(symbols[i], "td")[4])
        rev_act <- as.character(html_nodes(symbols[i], "td")[5])
        rev_for  <- as.character(html_nodes(symbols[i], "td")[6])
        market_cap  <- as.character(html_nodes(symbols[i], "td")[7])
        symbol_ticker <- as.character(html_nodes(symbols[i], "a"))
        #spliting
        country <- strsplit(country, '=|class')[[1]][2]
        symbol_name <- strsplit(symbol_name, '>|<')[[1]][5]
        eps_act <- strsplit(eps_act, '>|<')[[1]][3]
        eps_for <- strsplit(eps_for, '>|<')[[1]][3]
        rev_act <- strsplit(rev_act, '>|<')[[1]][3]
        rev_for <- strsplit(rev_for, '>|<')[[1]][3]
        rev_for <- substr(rev_for,4,10)
        eps_for <- substr(eps_for,4,10)
        market_cap  <- strsplit(market_cap, '>|<')[[1]][3]
        symbol_ticker <- strsplit(symbol_ticker, '>|<')[[1]][3]
        record = data.table(country=country,
                            name=symbol_name,
                            symbol=symbol_ticker,
                            date = date,
                            eps_actual = eps_act,
                            eps_forecast= eps_for,
                            rev_actual = rev_act,
                            rev_forecast = rev_for,
                            market_cap = market_cap)
        if(i==2)
          Records <- record
        if(i!=2)
          Records <- rbind(Records,record)
      }
    }
  }
  Sys.setlocale("LC_TIME", current_locale)
  if(status_code(r) != 200)
  {
    Records <- 'cannot connect to server'
  }
  if(length(symbols)>1)
  {
    if(exists('Records_result'))
      Records_result <- rbind(Records_result,Records)
    if(!exists('Records_result'))
      Records_result <- Records
  }
  }
  return(unique(Records_result))
}





