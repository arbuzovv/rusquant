

library(devtools)
library(xts)
library(TTR)
library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
devtools::install_github('arbuzovv/rusquant')

# token data for Finam Hackaton
rusquant_token = 'AAHXE1642J'

# get list of alpha performance
alpha_performance = getSymbolList(src='Rusquant', api.key=rusquant_token)

# choose alpha
alpha_universe = alpha_performance[sharpe_3y>0.8 & sharpe_1y>0.8 & sharpe_1y<3  & sharpe_1m<3 & ret_1m>0]

# add alpha data
for(i in 1:nrow(alpha_universe))
{
  print(alpha_universe[i])
  alpha_i = getSymbols.Rusquant(alpha_universe$symbol[i],field = alpha_universe$alpha[i],from = '2010-01-01',to=Sys.Date(),api.key = rusquant_token)
  price = getSymbols.Finam(alpha_universe$symbol[i],from = '2010-01-01',auto.assign = F)
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
# run optimizer
opt_portf <- optimize.portfolio(R=na.omit(res[time_period]), portfolio=pf,
                                   optimize_method="ROI",
                                   maxSR=TRUE, trace=TRUE)

# chart mean-var plot
meanvar.ef <- create.EfficientFrontier(R=na.omit(res[time_period]), portfolio=pf, type="mean-StdDev",n.portfolios = 300)
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
table.CalendarReturns(na.omit(portfolio_opt), digits = 1, as.perc = TRUE, geometric = TRUE)


# get signals for current date
for(i in 1:nrow(alpha_universe))
{
  alpha_i = getSymbols.Rusquant(alpha_universe$symbol[i],field = alpha_universe$alpha[i],from = '2010-01-01',to=Sys.Date(),api.key = rusquant_token)
  if(i==1) signals = data.table(alpha_universe$symbol[i],alpha_universe$alpha[i],tail(alpha_i,1)$signal)
  if(i!=1) signals = rbind(signals,data.table(alpha_universe$symbol[i],alpha_universe$alpha[i],tail(alpha_i,1)$signal))
}

signals$w = signals$V3*w_opt
position_per_symbol = signals[,sum(w,na.rm = T),by='V1']
names(position_per_symbol) = c('symbol','w')


##################### Trading execution logic ##############################
leverage = 4
finam_token = 'set_your_finam_token'
finam_account = 'set_your_finam_account'
finam_universe = data.table(getSymbolList(src = 'Finam',api.key = finam_token))

# actual portfolio
finam_portfolio = getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)
capital = finam_portfolio$money$balance * leverage # get from Portfolio
position_per_symbol$money = capital*position_per_symbol$w

# theoretical portfolio
trade_positions = position_per_symbol[w!=0]
universe_trade = unique(trade_positions$symbol)
futures_type = '6.23'
trade_positions$futures_code = finam_universe[shortName %in% paste(universe_trade,futures_type,sep = '-')]$code
# calc position size
trade_positions$size = 0
for(i in 1:nrow(trade_positions))
{
  trade_symbol = trade_positions$futures_code[i]
  last_price = tail((getSymbols.Finam(trade_symbol,period = '1min',from=Sys.Date()-2))[,4],1)
  trade_positions$size[i] = abs(as.numeric(ceiling(trade_positions$money[i]/last_price)))
}

# diff between theoretical and actual portfolio
change_portfolio = merge(trade_positions,finam_portfolio$positions[c('securityCode','balance')],by.x = 'futures_code',by.y = 'securityCode',all = T)
change_portfolio[is.na(size)]$size = 0
change_portfolio[is.na(balance)]$balance = 0
change_portfolio$trade_size = change_portfolio$size - change_portfolio$balance

#get current positions
for(i in 1:nrow(change_portfolio))
{
  trade_symbol = change_portfolio$futures_code[i]
  last_price = tail((getSymbols.Finam(trade_symbol,period = '1min',from=Sys.Date()-2))[,4],1)
  size_order = abs(as.numeric(change_portfolio$trade_size[i]))
  trade_side = ifelse(change_portfolio$trade_size[i]>0,'Buy','Sell')
  myorder = placeOrder(src = 'finam',
                       symbol = trade_symbol,
                       board = 'FUT',
                       action = trade_side,
                       totalQuantity = size_order,
                       lmtPrice = as.numeric(last_price),
                       api.key = finam_token,
                       clientId = finam_account)
}


# wait 1 minute and resend order
Sys.sleep(60)
# order routing logic
my_orders = getOrders(src = 'finam',api.key = finam_token)$orders
for(i in nrow(my_orders))
{
  # if not executed
  if(my_orders$status[i] == 'Active')
  {
    cancelOrder(src = 'finam',api.key = finam_token,orderId = my_orders$orderNo[i] ,clientId = finam_account)
    last_price = tail((getSymbols.Finam(trade_symbol,period = '1min',from=Sys.Date()-2))[,4],1)
    neworder = placeOrder(src = 'finam',
                         symbol = my_orders$securityCode[i] ,
                         board = 'FUT',
                         action = my_orders$buySell[i] ,
                         totalQuantity = my_orders$balance[i] ,
                         lmtPrice = as.numeric(last_price),
                         api.key = finam_token,
                         clientId = finam_account)
  }
}
# save day trading data
last_portfolio = getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)$positions
last_transactions = getOrders(src = 'Finam',api.key = finam_token)$orders

fwrite(last_portfolio,file = paste(Sys.Date(),'positions.csv',sep = '-'))
fwrite(last_transactions,file = paste(Sys.Date(),'transactions.csv',sep = '-'))


