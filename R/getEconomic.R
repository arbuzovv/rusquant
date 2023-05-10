#' @title Get economic data from Investing.com
#'
#' @description This function retrieves economic data from the investing.com website for a specified time period
#'
#' @param from A date in the format of YYYY-MM-DD. Defaults to 10 days ago from the current system date.
#' @param to A date in the format of YYYY-MM-DD. Defaults to the current system date.
#' @param country a character string with the country name to filter dividends data for (only for Investing.com)
#' @return A data frame containing IPO calendar data for the specified date range.
#' @note Not for the faint of heart. All profits and losses related are yours and yours alone. If you don't like it, write it yourself.
#' @author Vyacheslav Arbuzov
#' @examples
#' getEconomic(from = Sys.Date(),to = Sys.Date()+35,country='Belgium')
#' @export


"getEconomic" <- function(from=Sys.Date()-10,to=Sys.Date(),country = 'United States')
{
  url = 'https://www.investing.com/economic-calendar/Service/getCalendarFilteredData'
  end_date = to
  date = from
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
  data = paste0(paste0('country%5B%5D=',id_country,'&',collapse = ''),
                'dateFrom=',date,'&dateTo=',end_date,'&currentTab=custom&submitFilters=1&limit_from=0')

  headers = add_headers('Host' = 'www.investing.com',
                        'Origin' = 'https://www.investing.com',
                        'Referer' = 'https://www.investing.com/economic-calendar/',
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
        time  <- as.character(html_nodes(symbols[i], "td")[1])
        event  <- as.character(html_nodes(symbols[i], "td")[4])
        event_act  <- ifelse(length(html_nodes(symbols[i], "td"))>4,as.character(html_nodes(symbols[i], "td")[5]),NA)
        event_for  <- ifelse(length(html_nodes(symbols[i], "td"))>5,as.character(html_nodes(symbols[i], "td")[6]),NA)
        event_prev  <- ifelse(length(html_nodes(symbols[i], "td"))>6,as.character(html_nodes(symbols[i], "td")[7]),NA)
        #spliting
        pattern <- "title=([^ ]+)"
        country <- str_match(country, pattern)[,2]
        time <- strsplit(time, '>|<')[[1]][3]
        event <- strsplit(event, '>|<')[[1]][5]
        event_act <- ifelse(length(html_nodes(symbols[i], "td"))>4,strsplit(event_act, '>|<')[[1]][3],NA)
        event_for <- ifelse(length(html_nodes(symbols[i], "td"))>5,substr(strsplit(event_for, '>|<')[[1]][3],2,10),NA)
        event_prev <- ifelse(length(html_nodes(symbols[i], "td"))>5,strsplit(event_prev, '>|<')[[1]][5],NA)
        record = data.table(country=trimws(country),
                            date=date,
                            time=time,
                            event=trimws(event),
                            event_act=event_act,
                            event_for = event_for,
                            event_prev = event_prev)
        if(i==2)
          Records <- record
        if(i!=2)
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





