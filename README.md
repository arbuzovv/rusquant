# Rusquant
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rusquant)](https://cran.r-project.org/package=rusquant) [![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/last-month/rusquant)](https://cran.r-project.org/package=rusquant) [![CRAN_Ago](https://www.r-pkg.org/badges/ago/rusquant)](https://cran.r-project.org/package=rusquant)
### Intro
Rusquant is a package for interaction with alternative data, trading API of different exchanges and brokers. Package provides access to market data for storage, analysis, algorithmic trading, strategy backtesting. Also this is data downloader from different data sources starting from close price to order book and tradelog.

Current available brokers - [tinkoff.ru](https://www.tinkoff.ru), [finam.ru](https://www.finam.ru), [alorbroker.ru](https://alorbroker.ru).
Current available datasources - previous brokers + [MOEX](https://www.moex.com),  [MFD.RU](https://www.mfd.ru),  [Poloniex](https://www.poloniex.com),  [MarketWatch](https://www.marketwatch.com),  [Investing](https://www.investing.com), [AlgoPack](https://www.moex.com/ru/algopack)

### Supporting rusquant development

If you are interested in supporting the ongoing development and maintenance of rusquant, please consider [becoming a sponsor](https://boosty.to/rusquant/donate).

### Installation

The current release  (1.0.5) is available on [CRAN](https://CRAN.R-project.org/package=rusquant),
which you can install via:

```r
install.packages("rusquant")
```

To install the development version (1.0.5), you need to clone the repository and build
from source, or run one of:

```r
# lightweight
remotes::install_github("arbuzovv/rusquant")
# or
devtools::install_github("arbuzovv/rusquant")
```
### Getting Started

It is possible to import data from a variety of sources with one rusquant
function: `getSymbols()`. For example:

``` r
library(rusquant)
getSymbolList('Finam') # download all available symbols in finam.ru 
getSymbols('LKOH',src='Finam') # default = main market
getSymbols('LKOH',src = 'Finam') # main market
getSymbols.Moex('SBER')

# type period
getSymbols('LKOH',src='Finam',period='day') # day bars - default parameter
getSymbols('LKOH',src='Finam',period='5min') # 5 min bar 
getSymbols('LKOH',src='Finam',period='15min') # 15 min bar
getSymbols.Moex('SBER',period = '1min',from=Sys.Date()-20) # 1 min bar 
```

### Get data from [Mfd.ru](https://mfd.ru/export/)

``` r
getSymbolList('Mfd') # see the availible assets
getSymbols('Сбербанк',src='Mfd')
```

### Get fundamental data from [Investing](https://investing.com)

``` r
getEarnings(from = Sys.Date(),to = Sys.Date()+3,country='Belgium')
getEconomic(from = Sys.Date() - 10, to = Sys.Date(), country = "United States")
getIPO(from='2023-01-23',to='2023-01-25')
getDividends(from = Sys.Date(),to = Sys.Date()+2,country = "Australia")
getDividends(from = '2023-08-01',to = '2023-08-05',country = 'United States')
```

### Get microstructure data from [AlgoPack](https://www.moex.com/ru/algopack)

``` r
getSymbols.Algopack('SBER',from = '2023-10-24',to='2023-11-04')
getSymbols.Algopack('ROSN',from = '2023-10-24',to='2023-11-04',type = 'obstats')

```

### Live trading using broker account [Finam](https://finam.ru)

``` r
finam_token = 'mytoken'
finam_account = 'accountid'

#get portfolio info
finam_portfolio = getPortfolio(src = 'Finam',api.key = finam_token,clientId = finam_account)
current_balance = finam_portfolio$equity
finam_universe = data.table(getSymbolList(src = 'Finam',api.key = finam_token))
#change positions
trade_symbol = 'SBER'
last_price = try(tail((getSymbols.Finam(trade_symbol,period = '1min',from=Sys.Date()-2))[,4],1),silent = T)
size_order = 1
trade_side = ifelse(size_order>0,'Buy','Sell')
board = 'TQBR'

myorder = placeOrder(src = 'finam',
                     symbol = trade_symbol,
                     board = board,
                     action = trade_side,
                     totalQuantity = size_order,
                     lmtPrice = as.numeric(last_price),
                     api.key = finam_token,
                     clientId = finam_account)
print(myorder)

my_orders_status = getOrders(src = 'finam',api.key = finam_token,clientId = finam_account)$orders
print(my_orders_status)
```

### Author

[Vyacheslav Arbuzov](https://t.me/arbuzovv)
