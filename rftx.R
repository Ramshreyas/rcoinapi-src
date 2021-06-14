library(httr)
library(jsonlite)

BASE_URL <- "ftx.com/api"
MARKETS_ENDPOINT <- "/markets"
FUTURES_ENDPOINT <- "/futures"
EXPIRED_FUTURES_ENDPOINT <- "/expired_futures"
INDEXES_ENDPOINT <- "/indexes"

#----------------MARKETS---------------------------------------------------------------------------------------------

getMarkets <- function() {
  
  ftxPublicRequest("GET", MARKETS_ENDPOINT)
  
}

getMarket <- function(symbol) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol)
  
  ftxPublicRequest("GET", endpoint)
  
}

getOrderbook <- function(symbol, depth = 20) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/orderbook?depth=", depth)
  
  ftxPublicRequest("GET", endpoint)
  
}

getTrades <- function(symbol, start_time_millis, end_time_millis) {

  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/trades")
  
  ftxPublicRequest("GET", endpoint, params = paginate(start_time_millis, end_time_millis))
  
}

getHistoricalPrices <- function(symbol, resolution, start_time_millis, end_time_millis) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/candles")
  
  ftxPublicRequest("GET", endpoint, params = paginate(start_time_millis, end_time_millis, resolution))
}

#----------------FUTURES---------------------------------------------------------------------------------------------

getFutures <- function() {
  
  
}

getFuture <- function(symbol) {}

getFutureStats <- function(symbol) {}

getFundingRates <- function(symbol, start_time_millis, end_time_millis) {}

getIndexWeights <- function(symbol) {}

getExpiredFutures <- function(symbol) {}

getHistoricalIndex <- function(symbol, resolution, start_time_millis, end_time_millis) {}

#----------------UTILS---------------------------------------------------------------------------------------------

ftxPublicRequest <- function(method, path, params = NULL, body = NULL, retries = 0) {
  
  BASE_URL <- "https://ftx.com/api"
  
  url <- paste0(BASE_URL, path)
  METHOD <- getFromNamespace(method, ns = 'httr')
    
  res <- tryCatch(
    
    METHOD(url,
           query = params,
           body
    ),
    
    error = function(e) e
    
  )
  
  parseJSONText(res)
  
}

parseJSONText <- function(res) {

  resText <- content(res, as = "text")
  
  fromJSON(resText)$result
  
}

paginate <- function(start_time_millis, end_time_millis, resolution. = NULL) {
  
  if(is.null(resolution.)) {
    
    list(start_time = start_time_millis,
         end_time = end__time_millis)
    
  } else {
    
    list(resolution = resolution.,
         start_time = start_time_millis,
         end_time = end__time_millis) 
  }
  
}