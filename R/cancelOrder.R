cancelOrder <- function(Connection, OrderID) 
{
    url <- paste0("https://hft-api.lykke.com/api/Orders/",OrderID, "/Cancel")
	resp <- httr::POST(url,
                     httr::add_headers(`api-key` = slot(Connection,'Key')),
                     encode = "json")					
    return(resp)
}
