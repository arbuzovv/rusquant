
# install.packages("rusquant") # if not installed
library(devtools)
library(xts)
library(TTR)
library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(rusquant)

# set params for your account
leverage = 4
finam_token = 'set_your_finam_token'
finam_account = 'set_your_finam_account'
# token data for Finam Hackaton
rusquant_token = 'AAHXE1642J'

##################### Trading execution logic ##############################

w_opt = fread('w_opt.csv')
alpha_universe = fread('alpha_universe.csv')

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
trade_positions$price = 0
trade_positions$size = 0
if(!exists('symbol_list_FINAM')) getSymbolList(src = 'Finam',auto.assign = TRUE)
for(i in 1:nrow(trade_positions))
{
  trade_symbol = trade_positions$futures_code[i]
  last_price = try(tail((getSymbols(trade_symbol,src='Finam',period = '1min',from=Sys.Date()-2,auto.assign = F))[,4],1),silent = T)
  if(class(last_price)[1] == "try-error")
    last_price = try(tail((getSymbols(trade_symbol,src='Moex',period = '1min',from=Sys.Date()-2,auto.assign = F)[,2]),1),silent = T)
  trade_positions$size[i] = abs(as.numeric(ceiling(trade_positions$money[i]/last_price)))
  trade_positions$price[i] = abs(as.numeric(last_price))
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
  last_price = try(tail((getSymbols.Finam(trade_symbol,period = '1min',from=Sys.Date()-2))[,4],1),silent = T)
  if(class(last_price)[1] == "try-error")
    last_price = try(tail((getSymbols(trade_symbol,src='Moex',period = '1min',from=Sys.Date()-2,auto.assign = F)[,2]),1),silent = T)

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



# order routing logic
my_orders = getOrders(src = 'finam',api.key = finam_token,clientId = finam_account)$orders
while(nrow(my_orders[my_orders$status=='Active',])!=0)
{
for(i in nrow(my_orders))
{
  # if not executed
  if(my_orders$status[i] == 'Active')
  {
    cancelOrder(src = 'finam',api.key = finam_token,orderId = my_orders$transactionId[i] ,clientId = finam_account)
    last_price = try(tail((getSymbols.Finam(my_orders$securityCode[i],period = '1min',from=Sys.Date()-2))[,4],1),silent = T)
    if(class(last_price)[1] == "try-error")
      last_price = try(tail((getSymbols(my_orders$securityCode[i],src='Moex',period = '1min',from=Sys.Date()-2,auto.assign = F)[,2]),1),silent = T)

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
# wait 1 minute and resend order
Sys.sleep(60)
my_orders = getOrders(src = 'finam',api.key = finam_token,clientId = finam_account)$orders
}


# save day trading data
last_portfolio = data.table(Sys.Date(),getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)$positions)
last_transactions = data.table(Sys.Date(),getOrders(src = 'finam',api.key = finam_token,clientId = finam_account)$orders)
last_equity = data.table(Sys.Date(),getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)$equity)

fwrite(last_portfolio,file = 'positions.csv',append = T)
fwrite(last_transactions,file = 'transactions.csv',append = T)
fwrite(last_equity,file = 'equity.csv',append = T)

