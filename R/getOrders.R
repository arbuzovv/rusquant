getOrders <- function(Connection) 
{
        url <- "https://hft-api.lykke.com/api/Orders"

        resp <- httr::GET(url,
                           httr::add_headers(`api-key` = slot(Connection,'Key')),
                           encode = "json")
        return(content(resp, "parsed"))
}