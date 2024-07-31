library(R6)
library(RCurl)
library(jsonlite)
library(lubridate)
library(data.table)


#pkgload::load_code(path = "./R/lookupTabs.R")
#pkgload::load_code(path = "./R/helpers.R")
source("./R/lookupTabs.R")
source("./R/helpers.R")

GCapiClientV2 <- R6Class(
  "GCapiClientV2",
  public = list(
    rest_url = "https://ciapi.cityindex.com/v2",
    session_id = NULL,
    username = NULL,
    session = NULL,
    trading_account_id = NULL,

    #' @description
    #' Initialize the GCapiClientV2 object and create a session.
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

      if (is.null(resp$session)) {
        cat("Response:\n")
        print(resp) # Print the full response for debugging
        stop("GCapiException: Login failed - ", toString(resp))
      }

      session <- resp$session
      headers <- c(
        'Content-Type' = 'application/json',
        'UserName' = username,
        'Session' = session
      )

      # Setting up the session
      self$session <- getCurlHandle(httpheader = headers)
      self$session_id <- session
    },

    #' @description
    #' Retrieve account information.
    #' @param get Optional. Specific information to retrieve.
    #' @return A list containing account information or a specific value if `get` is provided.
    get_account_info = function(get = NULL) {
      response <- getURL(paste0(self$rest_url, '/UserAccount/ClientAndTradingAccount'), curl = self$session)
      resp <- fromJSON(response)

      if (is.null(resp$tradingAccounts) || length(resp$tradingAccounts) == 0) {
        stop("GCapiException: No trading accounts found - ", toString(resp))
      }

      self$trading_account_id <- resp$tradingAccounts$tradingAccountId
      if (!is.null(get)) {
        return(resp$tradingAccounts[[1]][[get]])
      } else {
        return(resp)
      }
    }
  )
)

