Connect <- function(host='localhost',port='',key='',sign='')
{
structure(list(host=host,port=port,key=key,sign=sign),class='ConnectionToExchange')
}