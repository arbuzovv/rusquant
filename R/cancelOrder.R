cancelOrder <- function(Connection, OrderID) 
{
    url <- paste0("https://hft-api.lykke.com/api/Orders/",OrderID, "/Cancel")
	resp <- httr::POST(url,
                     httr::add_headers(`api-key` = conn$key),
                     encode = "json")					
    return(resp)
}
