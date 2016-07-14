"getOptionChain.Forts" <-
function(Symbols, Exp, session='MAIN', verbose=FALSE)
{
     forts.URL <- "http://rts.micex.ru/ru/derivatives/optionsdesk-csv.aspx?sub=&marg=1&c2=on&c4=on&c6=on&c7=on"

     dlv <- format(as.Date(Exp,origin='1970-01-01'), '%d-%m-%y')

     sid <- 1
     if ('EVENING' == session){
         sid <- 2
     }

     tmp <- tempfile()
     stock.URL <- paste(forts.URL,
                           "&code=",Symbols,
                           "&sid=",sid,
                           "&delivery=",dlv,
                           sep='')
     download.file(stock.URL, destfile=tmp, quiet=!verbose)

     fr <- read.csv(tmp, as.is=TRUE, skip=1)
     unlink(tmp)

     dlv <- format(as.Date(Exp,origin='1970-01-01'), '%d%m%y')
     cnames <- paste(gsub('[ -.]','',Symbols), dlv, 'CA', fr[,10], sep='')
     rnames <- paste(gsub('[ -.]','',Symbols), dlv, 'PA', fr[,10], sep='')

     calls <- as.matrix(fr[,c(10, 4, 6, 7, 8, 1, 3, 11)]  )
     puts <-  as.matrix(fr[,c(10, 15, 17, 13, 14, 19, 18, 11)]  )
     calls <- apply(calls, 2, function(x) suppressWarnings(as.numeric(x)))
     puts <- apply(puts, 2, function(x) suppressWarnings(as.numeric(x)))
     colnames <- c("Strike","Last","Chg","Bid","Ask","Vol","OI", "IV")

     colnames(calls) <- colnames
     rownames(calls) <- cnames
     colnames(puts) <- colnames
     rownames(puts) <- rnames

     list(calls=calls, puts=puts)

}

