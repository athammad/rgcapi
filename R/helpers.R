#' Get Instruction Status Description
#'
#' Retrieve the description of an instruction status code.
#' @export
#' @param status_code The status code to look up.
#' @return A character string with the description of the status code.
get_instruction_status_description <- function(status_code) {
  return(instruction_status_descriptions[[as.character(status_code)]])
}

#' Get Instruction Status Reason Description
#'
#' Retrieve the description of an instruction status reason code.
#' @export
#' @param reason_code The reason code to look up.
#' @return A character string with the description of the reason code.
get_instruction_status_reason_description <- function(reason_code) {
  return(instruction_status_reason_descriptions[[as.character(reason_code)]])
}

#' Get Order Status Descriptions
#'
#' Retrieve the description of an order status code.
#' @export
#' @param status_code The status code to look up.
#' @return A character string with the description of the status code.
get_order_status_descriptions <- function(status_code) {
  return(order_status_descriptions[[as.character(status_code)]])
}

#' Get Order Status Reason Descriptions
#'
#' Retrieve the description of an order status reason code.
#' @export
#' @param reason_code The reason code to look up.
#' @return A character string with the description of the reason code.
get_order_status_reason_descriptions <- function(reason_code) {
  return(order_status_reason_descriptions[[as.character(reason_code)]])
}

#' Get Order Action Type Descriptions
#'
#' Retrieve the description of an order action type code.
#' @export
#' @param status_code The status code to look up.
#' @return A character string with the description of the status code.
get_order_action_type_descriptions <- function(status_code) {
  return(order_action_type_descriptions[[as.character(status_code)]])
}

#' Generate Time Frame Intervals for API Requests
#'
#' This helper function generates start and stop timestamps to bypass the data point limitation of the Gain Capital API.
#' @export
#' @param n_months Number of months of data to retrieve.
#' @param by_time Time interval for data extraction (e.g., '15 mins').
#' @param n Maximum number of data points per request.
#' @return A list of start and stop timestamps.
#' @examples
#' \dontrun{
#' intervals <- extractEveryNth(n_months = 6, by_time = '15 mins', n = 3900)
#' }
extractEveryNth <- function(n_months = 6, by_time = '15 mins', n = 3900) {
  timeSeq <- seq((as.POSIXct(Sys.time(), tz = "UTC") - months(n_months)), as.POSIXct(Sys.time(), tz = "UTC"), by = by_time)
  idx <- seq(length(timeSeq), 1, by = -n)
  idx <- rev(idx)
  startStop <- timeSeq[idx]
  Map(c, na.omit(shift(startStop)), startStop[-1])
}
