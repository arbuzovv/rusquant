"getDividendsInternal" <- function(url){
    data <- read.csv(url, sep=';', as.is=TRUE, quote="")
    stock <- data.frame(data[,1], data[,2],
                as.Date(strptime(data[,3], "%d/%m/%Y")),
                as.Date(strptime(data[,4], "%d/%m/%Y")),
                suppressWarnings(as.numeric(data[,5])),
                suppressWarnings(as.numeric(data[,7])),
                stringsAsFactors=FALSE)
    priv <- data.frame(paste(data[,1],'P', sep=''), data[,2],
                as.Date(strptime(data[,3], "%d/%m/%Y")),
                as.Date(strptime(data[,4], "%d/%m/%Y")),
                suppressWarnings(as.numeric(data[,6])),
                suppressWarnings(as.numeric(data[,8])),
                stringsAsFactors=FALSE)
    colnames(stock) <- c('Symbol', 'Name', 'BoardDate', 'RegistryDate', 'RecommendedDivs', 'Divs')
    colnames(priv) <- c('Symbol', 'Name', 'BoardDate', 'RegistryDate', 'RecommendedDivs', 'Divs')
    res <- rbind(stock, priv)

    return(res[complete.cases(res),])
}


"getAllDividends" <- function(allYears=FALSE){
    modern <- getDividendsInternal('http://gmi.troika.ru/eth/dividends/dividends.csv')
    if (allYears){
        old <- getDividendsInternal('http://gmi.troika.ru/eth/dividends/dividends_archive.csv')
        return(rbind(modern, old))
    }
    return(modern)
}



