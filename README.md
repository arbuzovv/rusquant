# rusquant

## Intro

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rusquant)](https://cran.r-project.org/package=rusquant) [![CRAN_Downloads](https://cranlogs.r-pkg.org/badges/last-month/rusquant)](https://cran.r-project.org/package=rusquant) [![CRAN_Ago](https://www.r-pkg.org/badges/ago/rusquant)](https://cran.r-project.org/package=rusquant)

Rusquant is a package for interaction with alternative data, trading API of different exchanges and trading terminals. Package provides access to market data for storage, analysis, algorithmic trading, strategy backtesting. Also this is data downloader from different data sources starting from close price to order book and tradelog.

Current available data sources - [tinkoff.ru](https://www.tinkoff.ru), [finam.ru](https://www.finam.ru), [mfd.ru](http://mfd.ru), [alorbroker.ru](https://alorbroker.ru).

### Supporting rusquant development

If you are interested in supporting the ongoing development and maintenance of rusquant, please consider [becoming a sponsor](https://boosty.to/rusquant/donate).

## Installation

The current release is available on [CRAN](https://CRAN.R-project.org/package=rusquant),
which you can install via:

```r
install.packages("rusquant")
```

To install the development version, you need to clone the repository and build
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
getSymbols('LKOH',src = 'Finam',market=1) # main market
getSymbols('LKOH',src = 'Finam',market=8) # ADR of LKOH, from market id from loadSymbolList

# type period
getSymbols('LKOH',src='Finam',period='day') # day bars - default parameter
getSymbols('LKOH',src='Finam',period='5min') # 5 min bar 
getSymbols('LKOH',src='Finam',period='15min') # 15 min bar 
```

## get data from [Mfd.ru](http://mfd.ru/export/)

``` r
library(rusquant)
getSymbols('Сбербанк',src='Mfd')
```

### Author

[Vyacheslav Arbuzov](https://t.me/arbuzovv)
