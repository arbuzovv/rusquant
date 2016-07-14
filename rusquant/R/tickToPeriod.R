"tickToPeriod" <- function(data, period, k=1){

    ep0 <- endpoints(data, period, k=k)
    ep <- cbind(Lag(ep0), ep0)[-1,]

    res <- apply(ep, 1, function(d) {
        x<-data[d[1]:d[2]];
        open <- as.numeric(Cl(data[d[1]+1]))
        close <- as.numeric(Cl(data[d[2]]))
        mean <- sum(Vo(x)*Cl(x))/sum(Vo(x))
        stddev <- sqrt(sum(((Cl(x)-mean)^2)*Vo(x))/(sum(Vo(x))))
        max <- max(Cl(x))
        min <- min(Cl(x))
        vol <- sum(Vo(x))
        return(c(open, max, min, close, vol, mean, stddev))
    })

    res <- aperm(res)
    res <- cbind(Cl(data)[ep0[1:(length(ep0)-1)]+1], res)[,-1]
    names(res) <- c("Open", "High", "Low", "Close", "Volume", "Mean", "Std")
    return(res)
}