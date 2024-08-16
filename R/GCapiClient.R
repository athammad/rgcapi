library(R6)
library(RCurl)
library(jsonlite)
library(data.table)



pkgload::load_code(path = "./R/lookupTabs.R")
pkgload::load_code(path = "./R/helpers.R")


#' GCapiClient
#'
#' This R6 class provides methods to interact with the Gain Capital API V1 for trading on Forex.com.
#' @export
#' @docType class
#' @format An \code{\link{R6Class}} generator object
#' @field rest_url The base URL for the Gain Capital API.
#' @field session_id The session ID for the current session.
#' @field username The username for the current session.
#' @field session The CURL handle for the current session.
#' @field trading_account_id The trading account ID for the current session.
#'
#' @importFrom R6 R6Class
#' @importFrom RCurl getCurlHandle
#' @importFrom RCurl postForm
#' @importFrom RCurl getURL
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' @importFrom data.table setDT
#' @importFrom data.table as.data.table shift
#' @importFrom stats na.omit
#' @importFrom pkgload load_code
#'
#' @examples
#' \dontrun{
#' gc_client <- GCapiClient$new("your_username", "your_password", "your_appkey")
#' account_info <- gc_client$get_account_info()
#' market_info <- gc_client$get_market_info("EUR/USD")
#' }
#'



GCapiClient <- R6Class(
  "GCapiClient",
  public = list(
    rest_url = "https://ciapi.cityindex.com/TradingAPI",
    session_id = NULL,
    username = NULL,
    session = NULL,
    trading_account_id = NULL,

    #' @description
    #' Initialize the GCapiClient object and create a session.
    #' @param username The username for the Gain Capital API.
    #' @param password The password for the Gain Capital API.
    #' @param appkey The application key for the Gain Capital API.
    #' @param proxies Optional. A list of proxy settings.
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

    #' @description
    #' Retrieve account information.
    #' @param get Optional. Specific information to retrieve.
    #' @return A list containing account information or a specific value if `get` is provided.
    get_account_info = function(get = NULL) {
      response <- getURL(paste0(self$rest_url, '/UserAccount/ClientAndTradingAccount'), curl = self$session)
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

    #' @description
    #' Retrieve market information.
    #' @param market_name The name of the market to retrieve information for.
    #' @param get Optional. Specific information to retrieve.
    #' @return A list containing market information or a specific value if `get` is provided.
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

    #' @description
    #' Retrieve price data for a market.
    #' @param market_id The market ID to retrieve prices for.
    #' @param num_ticks The number of price ticks to retrieve.
    #' @param from_ts The start timestamp in UTC.
    #' @param to_ts The end timestamp in UTC.
    #' @param price_type The type of price to retrieve ("MID", "BID", "ASK").
    #' @return A data.table containing the price data.
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

    #' @description
    #' Retrieve OHLC data for a market.
    #' @param market_id The market ID to retrieve OHLC data for.
    #' @param num_ticks The number of ticks to retrieve.
    #' @param interval The interval for OHLC data ("MINUTE", "HOUR", "DAY").
    #' @param span The span of the interval.
    #' @param from_ts The start timestamp in UTC.
    #' @param to_ts The end timestamp in UTC.
    #' @return A data.table containing the OHLC data.
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

    #' @description
    #' Place a trade order.
    #' @param quantity The quantity to trade.
    #' @param offer_price The offer price for the trade.
    #' @param bid_price The bid price for the trade.
    #' @param direction The direction of the trade ("buy" or "sell").
    #' @param trading_acc_id The trading account ID.
    #' @param market_id The market ID.
    #' @param market_name The market name.
    #' @param stop_loss Optional. The stop loss price.
    #' @param take_profit Optional. The take profit price.
    #' @param trigger_price Optional. The trigger price.
    #' @param close Optional. If TRUE, close the trade.
    #' @param order_id Optional. The order ID.
    #' @param tolerance Optional. The price tolerance.
    #' @return A data.table containing the order details.
    trade_order = function(quantity, offer_price, bid_price, direction, trading_acc_id, market_id, market_name, stop_loss = NULL, take_profit = NULL, trigger_price = NULL, close = FALSE, order_id = NULL, tolerance = NULL) {
      endpoint <- '/order/newtradeorder'

      # Adjust bid and offer prices based on tolerance
      bid_price <- bid_price - (tolerance * 0.0001)
      offer_price <- offer_price + (tolerance * 0.0001)

      order_details <- list(
        MarketId = market_id,
        Direction = direction,
        Quantity = quantity,
        OfferPrice = offer_price,
        BidPrice = bid_price,
        TradingAccountId = trading_acc_id,
        MarketName = market_name,
        AutoRollover = FALSE,
        IfDone = list(),
        OcoOrder = NULL,
        Type = NULL,
        ExpiryDateTimeUTC = NULL,
        Applicability = NULL,
        TriggerPrice = trigger_price,
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
      if (length(resp$Actions != 0)) {
        order_action_type <- get_order_action_type_descriptions(resp$Actions$OrderActionTypeId)
        print(paste("Action:", order_action_type))
      }
      print(paste("Order ID:", resp$Orders$OrderId, "-", status_desc, "-", reason_desc))
      print(paste("Order Status:", order_status_desc, "-", order_reason_desc))
      order_details$OrderId <- resp$Orders$OrderId
      order_details <- as.data.table(order_details)
      return(order_details)
    },

    #' @description
    #' List open positions.
    #' @return A data.table containing the open positions.
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

    #' @description
    #' List active orders.
    #' @return A list containing the active orders.
    list_active_orders = function() {
      endpoint <- '/order/activeorders'

      # Create the request body
      request_body <- list(
        TradingAccountId = self$trading_account_id
      )
      body <- toJSON(request_body, auto_unbox = TRUE)

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
      return(resp)
    },

    #' @description
    #' Close all trades.
    #' @param tolerance The price tolerance.
    #' @return A list containing the responses for each closed trade.
    close_all_trades = function(tolerance) {
      open_positions <- self$list_open_positions()

      if (length(open_positions) == 0) {
        message("No open positions to close")
        return(NULL)
      }
      close_responses <- list()
      for (i in 1:nrow(open_positions)) {
        position <- open_positions[i,]
        market_id <- position$MarketId
        direction <- ifelse(position$Direction == "buy", "sell", "buy")
        quantity <- position$Quantity
        order_id <- position$OrderId
        market_name <- position$MarketName
        price <- position$Price

        close_resp <- self$trade_order(
          quantity = quantity,
          offer_price = price,
          direction = direction,
          trading_acc_id = self$trading_account_id,
          market_id = market_id,
          market_name = market_name,
          close = TRUE,
          order_id = order_id,
          tolerance = tolerance
        )
        close_responses[[i]] <- close_resp
      }

      return(close_responses)
    },

    #' @description
    #' Close all trades using provided open positions.
    #' @param open_positions A list of open positions.
    #' @param tolerance The price tolerance.
    #' @return A list containing the responses for each closed trade.
    close_all_trades_new = function(open_positions, tolerance) {
      open_positions <- rbindlist(open_positions)
      if (length(open_positions) == 0) {
        message("No open positions to close")
        return(NULL)
      }
      close_responses <- list()
      for (i in 1:nrow(open_positions)) {
        position <- open_positions[i,]
        market_id <- position$MarketId
        direction <- ifelse(position$Direction == "buy", "sell", "buy")
        quantity <- position$Quantity
        order_id <- position$OrderId
        market_name <- position$MarketName
        offer_price <- position$OfferPrice
        bid_price <- position$BidPrice

        close_resp <- self$trade_order(
          quantity = quantity,
          offer_price = offer_price,
          bid_price = bid_price,
          direction = direction,
          trading_acc_id = position$TradingAccountId,
          market_id = market_id,
          market_name = market_name,
          close = TRUE,
          order_id = order_id,
          tolerance = tolerance
        )
        close_responses[[i]] <- close_resp
      }

      return(close_responses)
    },

    #' @description
    #' Retrieve trade history.
    #' @param trading_account_id The trading account ID.
    #' @param from Optional. The start timestamp for trade history.
    #' @param max_results The maximum number of results to return.
    #' @return A data.table containing the trade history.
    get_trade_history = function(trading_account_id = self$trading_account_id, from = NULL, max_results = 100) {
      if (!is.null(from)) {
        from_utc <- as.numeric(as.POSIXct(from, tz = "UTC"))
      }
      endpoint <- paste0(self$rest_url, '/order/tradehistory?TradingAccountId=', trading_account_id, '&maxResults=', max_results)
      if (!is.null(from)) {
        endpoint <- paste0(endpoint, '&from=', round(from_utc))
      }

      response <- getURL(endpoint, curl = self$session)

      if (response == "") {
        stop("GCapiException: Received empty response from API")
      } else {
        resp <- fromJSON(response)
      }

      if (is.null(resp$TradeHistory) || length(resp$TradeHistory) == 0) {
        stop("GCapiException: No trade history found for trading account ID - ", trading_account_id)
      } else {
        data <- resp$TradeHistory
        setDT(data)
        data <- data[, .(OrderId, LastChangedDateTimeUtc, ExecutedDateTimeUtc, MarketName, Direction, OriginalQuantity, Price, RealisedPnl)]
        data$LastChangedDateTimeUtc <- as.POSIXct(as.numeric(gsub("\\D", "", data$LastChangedDateTimeUtc)) / 1000, origin = "1970-01-01", tz = "UTC")
        data$ExecutedDateTimeUtc <- as.POSIXct(as.numeric(gsub("\\D", "", data$ExecutedDateTimeUtc)) / 1000, origin = "1970-01-01", tz = "UTC")
        data$LastChangedDateTimeUtc <- as.POSIXct(data$LastChangedDateTimeUtc, tz = "Asia/Singapore")
        data$ExecutedDateTimeUtc <- as.POSIXct(data$ExecutedDateTimeUtc, tz = "Asia/Singapore")
        return(data)
      }
    },

    #' @description
    #' Retrieve long series data by bypassing the API limitation of max 4000 data points.
    #' @param market_id The market ID to retrieve the long series data for.
    #' @param n_months Number of months of data to retrieve.
    #' @param by_time Time interval for data extraction.
    #' @param n Maximum number of data points per request.
    #' @param interval Time interval for OHLC data ("MINUTE", "HOUR", "DAY").
    #' @param span Span of the interval.
    #' @return A data.table containing the long series data.
    get_long_series = function(market_id, n_months = 6, by_time = '15 mins', n = 3900, interval = "MINUTE", span = 15) {
      result <- extractEveryNth(n_months = n_months, by_time = by_time, n = n)
      cat("Getting", n_months, "months of data...\n")
      long_series <- lapply(result, function(x) {
        apiHist <- self$get_ohlc(market_id = market_id, num_ticks = n, interval = interval, span = span, from_ts = as.integer(x[1]), to_ts = as.integer(x[2]))
        setDT(apiHist)
        apiHist <- apiHist[, .(BarDate, Close)]
      })
      long_series <- rbindlist(long_series)
      long_series <- long_series[!duplicated(BarDate)]
      return(long_series)
    }
  )
)



