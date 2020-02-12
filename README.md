# rusquant

## Intro



Rusquant is a package for interaction with alternative data, trading API of different exchanges and trading terminals.
Package provides access to market data for storage, analysis, algorithmic trading, strategy backtesting.
Also this is data downloader from different data sources starting from close price to order book and tradelog. 


Current available data sources - investing.com, finam.ru, mfd.ru, hs.alorbroker.ru, coinmarketcap.com. 

Current available cryptoexhanges - poloniex, kraken, binance, bttrex, cex, gate, gatecoin, gdax, gemini, hitbtc, liqui, lykke, xbtce. 

Current available trading terminals - IB, Metatrader, Quik. 

## Installing in [R]

```R
# install.packages("devtools") # if not installed
# options(download.file.method = "libcurl") # if problems with "Error in utils::download.file(url, path...."

library(devtools)
install_github("arbuzovv/rusquant")
```

## rus

Проект rusquant – это  R пакет позволяющий взаимодействовать с общедоступными источниками финансовой информации, API различных бирж и торговых терминалов.

Пакет является свободным программным обеспечением и распространяется абсолютно бесплатно.
Официальная страница проекта на R-forge: http://r-forge.r-project.org/projects/rusquant/
Для знакомства c использованием пакета можно воспользоваться документацией: http://rusquant.ru/docs/

Практические примеры применения можно найти:

В блоге официального сайта: http://rusquant.ru/blog/
На персональной странице Сергея Едунова:http://www.algorithmist.ru/p/rusquant.html
В блоге сайта infoption.ru:  http://infoption.ru/search/rusquant

Наиболее подробная информация находится на сайте http://rusquant.ru


## get data from Finam.ru

```R
library(rusquant)
loadSymbolList('Finam') # download all available symbols in Finam.ru 
getSymbols('LKOH',src='Finam') # default = main market
getSymbols('LKOH',src = 'Finam',market=1) # main market
getSymbols('LKOH',src = 'Finam',market=8) # ADR of LKOH, from market id from loadSymbolList

#type period
getSymbols('LKOH',src='Finam',period='day') # day bars - default parameter
getSymbols('LKOH',src='Finam',period='5min') # 5 min bar 
getSymbols('LKOH',src='Finam',period='15min') # 15 min bar 
```
