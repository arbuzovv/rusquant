Connect <- function(host='localhost',port='',key='',sign='')
{
setClass("ConnectionToExchange", representation(Key = "character",Sign="character"))
conn <- new("ConnectionToExchange", Key = key)
return(conn)
}