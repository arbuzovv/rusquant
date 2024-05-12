# rusquant

## Intro

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rusquant)](https://cran.r-project.org/package=rusquant) [![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/last-month/rusquant)](https://cran.r-project.org/package=rusquant) [![CRAN_Ago](https://www.r-pkg.org/badges/ago/rusquant)](https://cran.r-project.org/package=rusquant)

Rusquant is a package for interaction with alternative data, trading API of different exchanges and trading terminals. Package provides access to market data for storage, analysis, algorithmic trading, strategy backtesting. Also this is data downloader from different data sources starting from close price to order book and tradelog.

Current available data sources - [tinkoff.ru](https://www.tinkoff.ru), [finam.ru](https://www.finam.ru), [mfd.ru](http://mfd.ru), [alorbroker.ru](https://alorbroker.ru).

``` r
library(rusquant)
getSymbolList('Finam') # download all available symbols in finam.ru 
getSymbols('LKOH',src='Finam') # default = main market
getSymbols('LKOH',src = 'Finam',market=1) # main market
getSymbols('LKOH',src = 'Finam',market=8) # ADR of LKOH, from market id from loadSymbolList

# type period
getSymbols('LKOH',src='Finam',period='day') # day bars - default parameter
getSymbols('LKOH',src='Finam',period='5min') # 5 min bar 
getSymbols('LKOH',src='Finam',period='15min') # 15 min bar 

# download list of Symbols
available_etf_list = c("FXMM", "FXCN", "FXIT", "FXJP", "FXDE", "FXUS", "FXAU", "FXUK", "FXRB", "FXRL", "FXRU")
getSymbols(available_etf_list,src='Finam') 
```

## get data from [Mfd.ru](http://mfd.ru/export/)

``` r
library(rusquant)
getSymbols('Сбербанк',src='Mfd')
```
