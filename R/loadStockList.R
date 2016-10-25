"loadStockList.Mfd" <-
    function (verbose = FALSE){
        library(RCurl)
        library(XML)
        tgroup<-c(5,8,1,7,3,4,2)
        
        url<-paste("http://mfd.ru/marketdata/?id=",tgroup, sep="")
        tickers<-matrix(ncol=4)
        for(i in 1:length(tgroup)){
            webpage<-htmlParse(url[i])
            df<-getNodeSet(webpage, "//option[@data-n]")
            df<-lapply(df,function(x){c(xmlAttrs(x),xmlValue(x))})
            df<-matrix(unlist(df,use.names=FALSE),ncol=3, byrow=TRUE)
            df<-cbind(tgroup[i], df)
            print(paste(url[i], "  ", nrow(df)))
            tickers<-rbind(tickers, df)
        }
        tickers<-tickers[-1,]
        res<-tickers
        return(res)
    }

"loadStockList.Finam" <-
    function (verbose = FALSE){
        stocklist.URL = 'http://www.finam.ru/cache/icharts/icharts.js'
        tmp <- tempfile()
        download.file(stocklist.URL, destfile=tmp,quiet=!verbose)
        fr <- readLines(con = tmp, warn=FALSE)
        unlink(tmp)
        ids <- sub("var .*?= \\[", "", fr[1])
        ids <- sub("\\];", "", ids)
        ids <- strsplit(ids, ",")
        
        markets <- sub("var .*?= \\[", "", fr[4])
        markets <- sub("\\];", "", markets)
        markets <- strsplit(markets, ",")
        
        names <- sub("var .*?= \\[", "", fr[3])
        names <- sub("\\];", "", names)
        names <- gsub("'", "", names)
        names <- strsplit(names, ",")
        names[[1]]->names
        #	names[-(8497)]->names
        res <- unlist(ids)
        
        data<-data.frame(names,res,markets)
        data[data[,3]!=3,]->data
        rbind(data[(data[,1]%in%data[data[,3]==1,1]) & data[,3]==1,],data[!(data[,1]%in%data[data[,3]==1,1]),])->data
        data[,2]->res
        names(res) <- data[,1]
        return(res)
    }


"loadStockList" <-
    function (src = 'MOEX',verbose = FALSE, market="shares"){
        #MOEX - Moscow Exchange - Московская биржа        
		if(src == 'MOEX'){
			# full list of parameters: http://moex.com/iss/index.xml (trade_engine_name, market_name)
			if(length(market) == 2)
				market_type <- market 
			if(length(market) == 1)
				{
				if(market == "shares")
					market_type <- c('stock','shares') # Фондовый рынок - рынок акций
				if(market == "bonds")
					market_type <- c('stock','bonds') # Фондовый рынок - рынок облигаций
				if(market == "index")
					market_type <- c('stock','index') # Индексы фондового рынка
				if(market == "forts_main")
					market_type <- c('futures','main') # Срочный рынок - Срочные инструменты
				if(market == "forts_futures")
					market_type <- c('futures','forts') # Срочный рынок - ФОРТС
				if(market == "forts_options")
					market_type <- c('futures','options') # Срочный рынок - Опционы ФОРТС			
				if(market == "currency")
					market_type <- c('currency','selt') # Валютный рынок - SELT
				if(market == "commodity")
					market_type <- c('commodity','futures') # Товарный рынок - Секция стандартных контрактов АО НТБ						
				}
			if(length(market) > 2)
				message('Unkown market')
				
			SymbolsList <- na.omit(data.frame(matrix(ncol = 7)))
			names(SymbolsList) <- c("SECID", "SHORTNAME", "NAME", "BOARDID", "decimals", "history_from", "history_till")			
			i <- 0
			#download list of instrument	
			while(length(SymbolsList[,1])%%100 == 0)
			{
			  list.URL = paste("http://moex.com/iss/history/engines/",market_type[1],"/markets/",market_type[2],"/listing.csv?start=",i*100,sep='')
			  tmp <- tempfile()
			  download.file(list.URL, destfile=tmp,quiet = TRUE)
			  SymbolsList <- rbind(SymbolsList,read.csv(tmp, sep=";", skip=2, quote = "", stringsAsFactors=FALSE))
			  i <- i+1
			  unlink(tmp)
			}
			return(SymbolsList)
		}
		if(src == 'Finam'){
			stocklist.URL = 'http://www.finam.ru/cache/icharts/icharts.js'
			tmp <- tempfile()
			download.file(stocklist.URL, destfile=tmp,quiet=!verbose)
			fr <- readLines(con = tmp, warn=FALSE)
			unlink(tmp)
			ids <- sub("var .*?= \\[", "", fr[1])
			ids <- sub("\\];", "", ids)
			ids <- strsplit(ids, ",")
			
			markets <- sub("var .*?= \\[", "", fr[4])
			markets <- sub("\\];", "", markets)
			markets <- strsplit(markets, ",")
			
			names <- sub("var .*?= \\[", "", fr[3])
			names <- sub("\\];", "", names)
			names <- gsub("'", "", names)
			names <- strsplit(names, ",")
			names[[1]]->names
			#	names[-(8497)]->names
			res <- unlist(ids)
			
			data<-data.frame(names,res,markets)
			data[data[,3]!=3,]->data
			rbind(data[(data[,1]%in%data[data[,3]==1,1]) & data[,3]==1,],data[!(data[,1]%in%data[data[,3]==1,1]),])->data
			data[,2]->res
			names(res) <- data[,1]
			return(res)
		}
		
	        	
        
        
        #FORTS
        if("FORTS" %in% market){
            
            list.URL<-"ftp://ftp.moex.com/pub/info/stats/forts/FORTS_LIST.TXT"
            tmp <- tempfile()
            download.file(list.URL, destfile=tmp)
            FORTSlist <- read.csv(tmp, sep=";",quote = "", stringsAsFactors=FALSE)
            unlink(tmp)
            dataFORTS<-data.frame(shortSymbol=FORTSlist$short_symbol, 
                                  shortName=FORTSlist$short_name_ru, 
                                  FORTSlist$short_name_en, 
                                  FORTSlist$symbol, 
                                  type=FORTSlist$forts_system,  
                                  FORTSlist$contract_type,
                                  deliveryDate= FORTSlist$delivery_date, 
                                  FORTSlist$strike_price, 
                                  FORTSlist$amount, 
                                  FORTSlist$option_type
            )
        }
        else
            dataFORTS<-dataSTOCK
        
        res<-merge(dataSTOCK, dataFORTS, all=TRUE)
        return(res)
    }
