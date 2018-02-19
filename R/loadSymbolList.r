


#function retrives All avalible instruments of exchange
"loadSymbolList" <- function #S3 function (Poloniex is a class of first argument)
(src='kraken',
 verbose=FALSE,
 auto.assign=FALSE,
 ...)
{
        importDefaults("loadSymbolList"); #rewrite default values if specified by setDefaults
        env <- environment()
         
		if(src == 'kraken')
			downloadUrl <- 'https://api.kraken.com/0/public/AssetPairs'
		if(src == 'poloniex')		
			downloadUrl <- 'https://poloniex.com/public?command=returnTicker'
                
        tmp <- tempfile()
		if(verbose == TRUE) print(paste('Downloading file from',downloadUrl))
        download.file(downloadUrl, destfile = tmp, quiet = TRUE,method = 'curl') #get JSON object
		date_time <- Sys.time()
        rawdata <- readLines(tmp) #read raw data from file
        #convert to JSON
		rawdata <- fromJSON(rawdata)
		
		if(src == 'kraken')
		{
		if(length(rawdata$error)==0)
		{
			for(i in 1:length(names(rawdata[[2]])))
			{
				raw_currency <- rawdata[[2]][[i]]		
				if(i == 1)
					rawdata_m <- data.frame(raw_currency[1],raw_currency[2],raw_currency[3],raw_currency[4],raw_currency[5],raw_currency[6],raw_currency[7],raw_currency[8],raw_currency[9])
				if(i != 1)
					rawdata_m <- rbind(rawdata_m,data.frame(raw_currency[1],raw_currency[2],raw_currency[3],raw_currency[4],raw_currency[5],raw_currency[6],raw_currency[7],raw_currency[8],raw_currency[9]))
			}	
		}
		}
		if(src == 'poloniex')
		{
			for(i in 1:length(names(rawdata)))
			{
				raw_currency <- rawdata[[i]]		
				names(rawdata)[i]
				if(i == 1)
					rawdata_m <- data.frame(names(rawdata)[i],raw_currency[1],raw_currency[2],raw_currency[3],raw_currency[4],raw_currency[5],raw_currency[6],raw_currency[7],raw_currency[8])
				if(i != 1)
					rawdata_m <- rbind(rawdata_m,data.frame(names(rawdata)[i],raw_currency[1],raw_currency[2],raw_currency[3],raw_currency[4],raw_currency[5],raw_currency[6],raw_currency[7],raw_currency[8]))
			}
			rawdata_m[,2:ncol(rawdata_m)] <- apply(rawdata_m[,2:ncol(rawdata_m)],2,function(x) as.numeric(as.character(x)))
		}
		
                result <-paste('symbol_list_',toupper(gsub('\\^','',src)),sep='_')
                if(auto.assign){
                
                        assign(result, rawdata_m,globalenv())
				}
        
        if(auto.assign){
                return(result)
        }
        
        return(rawdata_m)
}