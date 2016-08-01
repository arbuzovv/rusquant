"loadStockListMfd" <-
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

"loadStockListFinam" <-
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


"loadStockListMoex" <-
    function (verbose = FALSE, market=c("MOEX", "FORTS")){
        #STOCK
        if("MOEX" %in% market){
            list.URL = "http://www.moex.com/iss/rms/engines/stock/objects/marketrates.csv?iss.only=object&limit=unlimited&sort_column=SECID&sort_order=ASC&security_types=common_share,preferred_share,depositary_receipt,ofz_bond,cb_bond,subfederal_bond,municipal_bond,corporate_bond,exchange_bond,ifi_bond,euro_bond,public_ppif,interval_ppif,private_ppif,stock_mortgage,etf_ppif&board_groups=stock_tplus,stock_ndm_tplus,stock_small_tplus,stock_qnv_tplus,stock_ndm_qnv_tplus,stock_d_tplus,stock_d_ndm_tplus,stock_t0,stock_ndm,stock_qnv,stock_ndm_qnv,stock_d_ndm,stock_bonds_d_main,stock_bonds_d_ndm,stock_darkpool,stock_b_spob,stock_b_psau,stock_b_auct,stock_b_psbb,stock_b_aubb,stock_repo_na,stock_repo_adr,stock_repo_shares,stock_repo_bonds,stock_repo_qnv&index=&listname=1,2,3,_&collateral=0&currencyid=&lang=ru"
            tmp <- tempfile()
            download.file(list.URL, destfile=tmp)
            STOCKlist <- read.csv(tmp, sep=";", skip=2, quote = "", stringsAsFactors=FALSE)
            unlink(tmp)
            dataSTOCK<-data.frame(STOCKlist$ISIN, 
                                  shortSymbol=STOCKlist$SECID,
                                  shortName=STOCKlist$SHORTNAME, 
                                  longName=STOCKlist$NAME,  
                                  type=STOCKlist$TYPENAME,  
                                  deliveryDate=STOCKlist$SETTLEDATE,
                                  STOCKlist$REGISTRY_CLOSE_DATE, 
                                  STOCKlist$LISTLEVEL
            )
        }
        else
            dataSTOCK<-dataFORTS
        
        
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
