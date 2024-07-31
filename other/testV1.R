
source("./GCAPI/GCapiClient.R")


# Example usage:
client <- GCapiClient$new(username = keyring::key_get(service = "fx_sytem", username = "IDLOG"),
                          password = keyring::key_get(service = "fx_sytem", username = "PSWD"),
                          appkey = keyring::key_get(service = "fx_sytem", username = "APKEY"))

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

print(close_resp)

myPositions<-client$list_open_positions()

client$trade_order(
  quantity = myPositions$Quantity,
  offer_price = myPositions$Price,
  direction = ifelse(myPositions$Direction == "buy", "sell", "buy"),
  trading_acc_id = client$trading_account_id,
  market_id = myPositions$MarketId,
  market_name = myPositions$MarketName,
  close = TRUE,
  order_id = myPositions$OrderId,
  tolerance = 0.0005
)


# Get trade history
trade_history <- client$get_trade_history(from = Sys.time()-days(2))

# Calculate realized P&L
realized_pnl <- client$calculate_realized_pnl(trade_history)

head(trade_history,2)$RealisedPnl

sum(head(lient$get_trade_history(from = Sys.time()-days(2)),2)$RealisedPnl,na.rm = T)
