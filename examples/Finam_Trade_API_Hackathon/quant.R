

# run  if not installed
install.packages("rusquant")
install.packages("PerformanceAnalytics")
# packages for portfolio optimization
install.packages("PortfolioAnalytics")
install.packages("ROI")
install.packages("ROI.plugin.glpk")
install.packages("ROI.plugin.quadprog")


library(xts)
library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(rusquant)

# token data for Finam Hackaton
rusquant_token = 'AAHXE1642J'

# get list of alpha performance
alpha_performance = getSymbolList(src='Rusquant', api.key=rusquant_token)

# choose alpha
alpha_universe = alpha_performance[sharpe_3y>0.8 & sharpe_1y>0.8 & sharpe_1y<3  & sharpe_1m<3 & ret_1m>0 & ret_1m<0.4 & q<0.7]

# add universe of Finam for downloading data from Finam site
getSymbolList(src = 'Finam',auto.assign = TRUE)

# add alpha data
for(i in 1:nrow(alpha_universe))
{
  print(alpha_universe[i])
  alpha_i = getSymbols.Rusquant(alpha_universe$symbol[i],field = alpha_universe$alpha[i],from = '2010-01-01',to=Sys.Date(),api.key = rusquant_token)
  price = try(getSymbols.Finam(alpha_universe$symbol[i],from = '2010-01-01',auto.assign = F),silent = TRUE)
  if(class(price)[1] == 'try-error')
  {
    price = getSymbols.Moex(alpha_universe$symbol[i],from = '2010-01-01',auto.assign = F)
    price = xts(price[,c(1,3,4,2)],order.by = as.Date(price$timestamp))
  }
  ret = ROC(price[,4])
  future_ret = lag(ret,-1)
  alpha_signal = xts(alpha_i$signal,order.by = as.Date(alpha_i$date))
  alpha_signal_ret = merge(alpha_signal,future_ret)
  alpha_signal_ret$pnl = alpha_signal_ret[,1]*alpha_signal_ret[,2]
  alpha_signal_ret = na.omit(alpha_signal_ret)[,3]
  if(i==1) res = alpha_signal_ret
  if(i!=1) res = merge(res,alpha_signal_ret)
}

# all NA values = 0
for(i in 1:ncol(res)) res[is.na(res[,i]),i] <- 0

############# alpha portfolio constructions ###############
# each alpha as asset
alpha_universe$symbol_alpha = paste(alpha_universe$symbol,alpha_universe$alpha,sep = '_')
names(res) = alpha_universe$symbol_alpha

chart.CumReturns(res['2018/'][,1:7],legend.loc = 'topleft')

# create virtual object portfolio
pf = portfolio.spec(alpha_universe$symbol_alpha)

# add add.constraints
pf = add.constraint(portfolio=pf, type="full_investment")
pf = add.constraint(portfolio=pf, type="long_only")
pf = add.objective(portfolio=pf, type="return", name="mean")
pf = add.objective(portfolio=pf, type="risk", name="StdDev")
pf = add.constraint(portfolio=pf, type="box", min=0.01, max=0.1)

# add group constraint for symbol weight
# create groups by instrument
for(i in 1:length(unique(alpha_universe$symbol)))
{
  symbol_i = as.character(unique(alpha_universe$symbol)[i])
  list_symbol = list(alpha_universe[symbol==symbol_i,which = TRUE])
  if(i==1) group_list = list_symbol
  if(i!=1) group_list <- append(group_list,list_symbol)
}
names(group_list) = unique(alpha_universe$symbol)
pf <- add.constraint(portfolio=pf, type="group",
                        groups=group_list,
                        group_min = rep(0,length(group_list)),
                        group_max = rep(0.2,length(group_list)))

# choose period with current regime of market
time_period = '2010/'


# run optimizer - if some error - it's mean that there are no solutions with such alpha and constraints - please change alpha filters or constraints
opt_portf <- optimize.portfolio(R=na.omit(res[time_period]), portfolio=pf,
                                   optimize_method="ROI",
                                   maxSR=TRUE, trace=TRUE)

# chart mean-var plot
meanvar.ef <- create.EfficientFrontier(R=na.omit(res[time_period]), portfolio=pf, type="mean-StdDev",n.portfolios = 100,search_size = 500)
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l",pch.assets=16,cex.assets = 0.7,cex.legend = 0.1)
grid()
points(opt_portf$objective_measures$StdDev,opt_portf$objective_measures$mean,col='blue',pch=16,cex=2)


# weights for alpha strategy portfolio
w_opt = opt_portf$weights
as.matrix(w_opt)
summary(w_opt)
# calc total performance of alpha portfolio
portfolio_opt = xts(rowSums(res*w_opt),order.by = as.Date(index(res)))

# calc performance of Alpha Portfolio
# get maximum leverage = 4
charts.PerformanceSummary(4*portfolio_opt['2020/'],geometric = T)
SharpeRatio.annualized(portfolio_opt['2020/'])
CalmarRatio(portfolio_opt['2020/'])

names(portfolio_opt) = 'alpha_portfolio'
# table.CalendarReturns(na.omit(4*portfolio_opt), digits = 1, as.perc = TRUE, geometric = TRUE)

# save data for tradebot.R
fwrite(data.table(w_opt),file = 'w_opt.csv')
fwrite(alpha_universe,file = 'alpha_universe.csv')
