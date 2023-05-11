library(rusquant)

handler <- function(event, context) {
  #params of bot
  # trading bot with Sharpe ~ 1-1.5
  finam_token = 'set_your_finam_token'
  finam_account = 'set_your_finam_account'
  rusquant_token = 'AAHXE1642J'
  symbol = 'SBER'

  # get info
  signal = tail(getSymbols.Rusquant(symbol,field = 'A1_L_P1',from = '2020-01-01',to=Sys.Date(),api.key = rusquant_token)$signal,1)
  finam_portfolio = getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)
  finam_universe = data.table(getSymbolList(src = 'Finam',api.key = finam_token))
  price = getSymbols(symbol,src='Moex',from=Sys.Date(),auto.assign = F,period='1min')
  last_price = tail(price$close,1)
  capital = finam_portfolio$money$balance # get from Portfolio
  positions = finam_portfolio$positions
  lot_size = finam_universe[code == symbol & market=='Stock']$lotSize
  board = finam_universe[code == symbol & market=='Stock']$board

  # calc theor and practical position
  theor_symbol_position = signal * floor(capital/last_price/lot_size)
  pract_symbol_position = 0
  if(symbol %in% positions$securityCode)
  {
    pract_symbol_position = symbol_position = positions[positions$securityCode == symbol,]$balance
  }
  change_position =  theor_symbol_position - pract_symbol_position

  # if trade, send order
  if(change_position!=0)
  {
    trade_side = ifelse(change_position>0,'Buy','Sell')
    myorder = placeOrder(src = 'finam',
                         symbol = symbol,
                         board = board,
                         action = trade_side,
                         totalQuantity = change_position,
                         lmtPrice = as.numeric(last_price),
                         api.key = finam_token,
                         clientId = finam_account)
  }
  return(list(statusCode = 200, body = capital))
}
