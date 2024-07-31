library(R6)
library(RCurl)
library(jsonlite)
library(lubridate)
library(data.table)

# lookupTabs.R contains the necessary lookup tables
source("./GCAPI/lookupTabs.R")

GCapiClientv2 <- R6Class(
  "GCapiClient",
  public = list(
    rest_url = "https://ciapi.cityindex.com/v2",
    session_id = NULL,
    username = NULL,
    session = NULL,
    trading_account_id = NULL,

    initialize = function(username, password, appkey, proxies = NULL) {
      self$username <- username
      headers <- c('Content-Type' = 'application/json')
      data <- list(
        UserName = username,
        Password = password,
        AppKey = appkey
      )

      # Convert data to JSON format
      data_json <- toJSON(data, auto_unbox = TRUE)

      # Create a new curl handle
      curl_handle <- getCurlHandle()

      # Make the POST request to create a session
      response <- postForm(
        paste0(self$rest_url, '/session'),
        .opts = list(
          httpheader = headers,
          postfields = data_json,
          verbose = TRUE,
          ssl.verifypeer = TRUE # Enable SSL verification
        )
      )

      resp <- fromJSON(response)
      if (is.null(resp$Session)) {
        stop("GCapiException: Login failed - ", toString(resp))
      }

      session <- resp$Session
      headers <- c(
        'Content-Type' = 'application/json',
        'UserName' = username,
        'Session' = session
      )

      # Setting up the session
      self$session <- getCurlHandle(httpheader = headers)
      if (!is.null(proxies)) {
        curlSetOpt(.opts = list(proxy = proxies), curl = self$session)
      }
      self$session_id <- session
    },

    get_account_info = function(get = NULL) {
      response <- getURL(paste0(self$rest_url, '/useraccount/ClientAndTradingAccount'), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$TradingAccounts) || length(resp$TradingAccounts) == 0) {
        stop("GCapiException: No trading accounts found - ", toString(resp))
      }

      self$trading_account_id <- resp$TradingAccounts$TradingAccountId
      if (!is.null(get)) {
        return(resp$TradingAccounts[[1]][[get]])
      } else {
        return(resp)
      }
    },

    get_market_info = function(market_name, get = NULL) {
      response <- getURL(paste0(self$rest_url, '/cfd/markets?marketName=', URLencode(market_name)), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$Markets) || length(resp$Markets) == 0) {
        stop("GCapiException: No market information found for market name - ", market_name)
      }

      if (!is.null(get)) {
        return(resp$Markets[1,get])
      } else {
        return(resp$Markets[[1]])
      }
    },

    get_prices = function(market_id, num_ticks, from_ts, to_ts, price_type = "MID") {
      endpoint <- paste0('/market/', market_id, '/tickhistorybetween?fromTimeStampUTC=', from_ts, '&toTimeStampUTC=', to_ts, '&maxResults=', num_ticks, '&priceType=',toupper(price_type))
      response <- getURL(paste0(self$rest_url, endpoint), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$PriceTicks) || length(resp$PriceTicks) == 0) {
        stop("GCapiException: No price data found for market ID - ", market_id)
      }
      data <- resp$PriceTicks
      data$TickDate <- as.POSIXct(as.numeric(gsub("\\D", "", data$TickDate)) / 1000, origin = "1970-01-01", tz = "UTC")
      setDT(data)
      return(data)
    },

    get_ohlc = function(market_id = NULL, num_ticks = NULL, interval = "HOUR", span = 1, from_ts, to_ts) {
      endpoint <- paste0('/market/', market_id, '/barhistorybetween?interval=', interval, '&span=', span, '&fromTimeStampUTC=', from_ts, '&toTimeStampUTC=', to_ts, '&maxResults=', num_ticks)
      response <- getURL(paste0(self$rest_url, endpoint), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$PriceBars) || length(resp$PriceBars) == 0) {
        stop("GCapiException: No OHLC data found for market ID - ", market_id)
      }

      data <- resp$PriceBars
      data$BarDate <- as.POSIXct(as.numeric(gsub("\\D", "", data$BarDate)) / 1000, origin = "1970-01-01", tz = "UTC")
      setDT(data)
      return(data)
    },

    trade_order = function(quantity, offer_price, direction, trading_acc_id, market_id, market_name, stop_loss = NULL, take_profit = NULL, trigger_price = NULL, close = FALSE, order_id = NULL, tolerance = NULL) {
      endpoint <- '/order/newtradeorder'

      # Calculate BidPrice and OfferPrice with tolerance
      bid_price <- offer_price - 0.0002
      if (!is.null(tolerance)) {
        bid_price <- max(0, offer_price - tolerance)
        offer_price <- offer_price + tolerance
      }

      order_details <- list(
        MarketId = market_id,
        Direction = direction,
        Quantity = quantity,
        OfferPrice = offer_price,
        TradingAccountId = trading_acc_id,
        MarketName = market_name,
        AutoRollover = FALSE,
        IfDone = list(),
        OcoOrder = NULL,
        Type = NULL,
        ExpiryDateTimeUTC = NULL,
        Applicability = NULL,
        TriggerPrice = trigger_price,
        BidPrice = bid_price,
        PositionMethodId = 1,
        isTrade = TRUE
      )

      if (close) {
        order_details$Close <- list(order_id)
      }

      if (!is.null(stop_loss) || !is.null(take_profit)) {
        ifdone_order <- list(
          StopOrder = list(
            Price = stop_loss,
            Type = "stop",
            Applicability = "gtc",
            StopType = "loss"
          ),
          LimitOrder = list(
            Price = take_profit,
            Type = "limit",
            Applicability = "gtc"
          )
        )
        order_details$IfDone <- list(ifdone_order)
      }

      body <- toJSON(order_details, auto_unbox = TRUE)

      headers <- c('Content-Type' = 'application/json', 'UserName' = self$username, 'Session' = self$session_id)

      response <- postForm(
        paste0(self$rest_url, endpoint),
        .opts = list(
          httpheader = headers,
          postfields = body,
          ssl.verifypeer = TRUE # Enable SSL verification
        )
      )

      resp <- fromJSON(response)
      status_desc <- get_instruction_status_description(resp$StatusReason)
      reason_desc <- get_instruction_status_reason_description(resp$StatusReason)
      order_status_desc <- get_order_status_reason_descriptions(resp$Orders$StatusReason)
      order_reason_desc <- get_order_status_reason_descriptions(resp$Orders$StatusReason)
      if(length(resp$Actions!=0)){
        order_action_type<- get_order_action_type_descriptions(resp$Actions$OrderActionTypeId)
        print(paste("Action:",order_action_type))
      }
      print(paste("Order ID:",resp$Orders$OrderId,"-",status_desc,"-",reason_desc))
      print(paste("Order Status:",order_status_desc,"-",order_reason_desc))
      return(resp)
    },

    list_open_positions = function() {
      endpoint <- '/order/openpositions'
      response <- getURL(paste0(self$rest_url, endpoint), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$OpenPositions) || length(resp$OpenPositions) == 0) {
        message("GCapiException: No open positions found")
      }

      data <- resp$OpenPositions
      as.data.table(data)
      return(data)
    },

    close_all_trades = function() {
      open_positions <- self$list_open_positions()
      if (is.null(open_positions) || nrow(open_positions) == 0) {
        message("No open positions to close")
        return()
      }

      for (i in seq_len(nrow(open_positions))) {
        self$trade_order(
          quantity = open_positions$Quantity[i],
          offer_price = open_positions$Price[i],
          direction = ifelse(open_positions$Direction[i] == "buy", "sell", "buy"),
          trading_acc_id = self$trading_account_id,
          market_id = open_positions$MarketId[i],
          market_name = open_positions$MarketName[i],
          close = TRUE,
          order_id = open_positions$OrderId[i]
        )
      }
    },

    get_account_margin = function() {
      endpoint <- '/useraccount/ClientAccountMargin'
      response <- getURL(paste0(self$rest_url, endpoint), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$ClientAccountMargin)) {
        stop("GCapiException: Unable to retrieve account margin information")
      }

      margin_info <- resp$ClientAccountMargin
      list(
        AvailableToTrade = margin_info$AvailableToTrade,
        Cash = margin_info$Cash,
        Equity = margin_info$Equity,
        PnL = margin_info$PnL,
        Margin = margin_info$Margin,
        MarginIndicator = margin_info$MarginIndicator
      )
    }
  )
)

# Example usage:
IDLOG <- "ahmed.t.hammad@gmail.comhsec"
PSWD <- "Trade123@"
APKEY <- "Ah.Hammad"

client <- GCapiClientv2$new(username = IDLOG, password = PSWD, appkey = APKEY)
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
fromA <- as.integer(as.POSIXct(Sys.Date(), tz="UTC")-months(1))
toB <- as.integer(as.POSIXct(Sys.time(), tz="UTC"))
prices <- client$get_prices(market_id = market_id, num_ticks = 1, from_ts = fromA, to_ts = toB, price_type = "MID")
print(prices)

# Get OHLC data for a market
fromA <- as.integer(as.POSIXct(Sys.Date(), tz="UTC")-days(6))
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
  take_profit = 1.080000
)

# List open positions
open_positions <- client$list_open_positions()
print(open_positions)

# Close all open trades
client$close_all_trades()

# Get account margin information
margin_info <- client$get_account_margin()
print(margin_info)
