
#This one is taken from quanmod package since it's not available through the API
"convert.time.series" <-
        function (fr, return.class)
        {
                if ("quantmod.OHLC" %in% return.class) {
                        class(fr) <- c("quantmod.OHLC", "zoo")
                        return(fr)
                }
                else if ("xts" %in% return.class) {
                        return(fr)
                }
                if ("zoo" %in% return.class) {
                        return(as.zoo(fr))
                }
                else if ("ts" %in% return.class) {
                        fr <- as.ts(fr)
                        return(fr)
                }
                else if ("data.frame" %in% return.class) {
                        fr <- as.data.frame(fr)
                        return(fr)
                }
				else if ("data.table" %in% return.class) {
                        fr <- as.data.table(fr)
                        return(fr)
                }
                else if ("matrix" %in% return.class) {
                        fr <- as.data.frame(fr)
                        return(fr)
                }
                else if ("its" %in% return.class) {
                        if ("package:its" %in% search() || suppressMessages(require("its",
                                                                                    quietly = TRUE))) {
                                fr.dates <- as.POSIXct(as.character(index(fr)))
                                fr <- its::its(coredata(fr), fr.dates)
                                return(fr)
                        }
                        else {
                                warning(paste("'its' from package 'its' could not be loaded:",
                                              " 'xts' class returned"))
                        }
                }
                else if ("timeSeries" %in% return.class) {
                        if ("package:fSeries" %in% search() || suppressMessages(require("fSeries",  quietly = TRUE))) {
                                fr <- timeSeries(coredata(fr), charvec = as.character(index(fr)))
                                return(fr)
                        }
                        else {
                                warning(paste("'timeSeries' from package 'fSeries' could not be loaded:", " 'xts' class returned"))
                        }
                }
        }
