# rgcapi <img src="./logo_rgcapi.png" align="right" height="200"/>

The `rgcapi` package provides an interface to the **Gain Capital API** V1 and V2, enabling users to perform various trading operations on [Forex.com](forex.com). This package includes functionalities for account management, market information retrieval, trading operations, and historical data extraction. It also includes helper functions and lookup tables to facilitate the interpretation of API responses.

## Features

- **Account Management**: Initialize sessions and manage user accounts.
- **Market Information**: Retrieve real-time market data and information.
- **Trading Operations**: Execute trades, manage orders, and track positions.
- **Historical Data**: Extract and analyze historical market data.
- **Helper Functions**: Utilize various helper functions and lookup tables for easier API response interpretation.

## Installation

You can install this package directly from GitHub using `devtools`:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install rgcapi from GitHub
devtools::install_github("yourusername/rgcapi")
```

## API Credentials

To access your credentials, you will need to contact Forex.com, which will provide you with an *Id*, *Apikey*, and *Password*. Once you have them, you can use them to connect to your account as follows:

```r
# Replace with your actual credentials
IDLOG <- "your_username"
PSWD <- "your_password"
APKEY <- "your_appkey"

client <- GCapiClientV2$new(username = IDLOG, password = PSWD, appkey = APKEY)
```

The `R6` interface is the same between V1 and V2; only the class names are different.


## Example usage

```r
account_info <- client$get_account_info()
print(account_info)

#EUR/USD','EUR/SGD','EUR/HKD
#[402044081, 401203172, 401203168]
market_info <- client$get_market_info("EUR/USD")
print(market_info)

# Retrieve specific information (e.g., MarketId)
market_id <- client$get_market_info("EUR/USD", get = "MarketId")
market_name <- client$get_market_info("EUR/USD", get = "Name")
print(market_id)
print(market_name)

# Get price data for a market
fromA <- as.integer(as.POSIXct(Sys.Date(), tz="UTC") - months(1))
toB <- as.integer(as.POSIXct(Sys.time(), tz="UTC"))
prices <- client$get_prices(market_id = market_id, num_ticks = 1, from_ts = fromA, to_ts = toB, price_type = "MID")
print(prices)

# Get OHLC data for a market
fromA <- as.integer(as.POSIXct(Sys.Date(), tz="UTC") - days(1))
toB <- as.integer(as.POSIXct(Sys.time(), tz="UTC"))
ohlc <- client$get_ohlc(market_id = market_id, num_ticks = 4000, interval = "MINUTE", span = 30, from_ts = fromA, to_ts = toB)
print(ohlc)

# Place a new trade order
trade_resp <- client$trade_order(
  quantity = 1020,
  offer_price = prices$Price,
  direction = "buy",
  trading_acc_id = client$trading_account_id,
  market_id = market_id,
  market_name = market_name,
  stop_loss = 1.060000,
  take_profit = 1.080000,
  tolerance = 0.0005
)


# List open positions
open_positions <- client$list_open_positions()
print(open_positions)


# Close a trade order
close_resp <- client$trade_order(
  quantity = 1020,
  offer_price = prices$Price,
  direction = "sell",
  trading_acc_id = client$trading_account_id,
  market_id = market_id,
  market_name = market_name,
  close = TRUE,
  order_id = open_positions$OrderId[1]
)


# Get trade history
trade_history <- client$get_trade_history()

```

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/athammad/rgcapi/issues/).

## Author
`rgcapi` is written by [Ahmed T. Hammad](https://athsas.com/) and is under active development. Please feel free to contribute by submitting any issues or requests—or by solving any current issues!


## Disclaimer
This package is not supported by `Forex.com`, and the author does not hold any responsibility for how users decide to use the library. Use it at your own risk.


## Official API documentation

https://docs.labs.gaincapital.com/index.html
