library(httr)
library(jsonlite)

BASE_URL <- "https://rest.coinapi.io"
EXCHANGES_ENDPOINT <- "/v1/exchanges/"
ASSETS_ENDPOINT <- "/v1/assets/"
SYMBOLS_ENDPOINT <- "/v1/symbols/"
EXCHANGERATE_ENDPOINT <- "/v1/exchangerate/"
OHLCV_ENDPOINT <- "/v1/ohlcv/"
TRADES_ENDPOINT <- "/v1/trades/"
QUOTES_ENDPOINT <- "v1/quotes/"
ORDERBOOKS_ENDPOINT <- "/v1/orderbooks/"
ORDERBOOKL3_ENDPOINT <- "/v1/orderbooks3/"

#----------------MARKETS---------------------------------------------------------------------------------------------

getExchanges <- function(exchangeId = NULL) {
  
  if(length(exchangeId = 1)) {
    
    endpoint <- paste0(EXCHANGES_ENDPOINT, exchangeId)
    
    executeRequest("GET", endpoint)
    
  } else {
    
    endpoint <- paste0(EXCHANGES_ENDPOINT)
    
    executeRequest("GET", endpoint, list)
    
  }
  
}

getMarket <- function(symbol) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol)
  
  executeRequest("GET", endpoint)
  
}

getOrderbook <- function(symbol, depth = 20) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/orderbook?depth=", depth)
  
  executeRequest("GET", endpoint)
  
}

getTrades <- function(symbol, start_time_millis, end_time_millis) {

  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/trades")
  
  executeRequest("GET", endpoint, params = paginate(start_time_millis, end_time_millis))
  
}

getHistoricalPrices <- function(symbol, resolution, start_time_millis, end_time_millis) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/candles")
  
  executeRequest("GET", endpoint, params = paginate(start_time_millis, end_time_millis, resolution))
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

executeRequest <- function(method, path, params = NULL, body = NULL, retries = 0) {
  
  apiKey <- Sys.getenv("COIN_API_KEY")
  
  if(apiKey == "") stop("No API Key!! Please set CoinAPI key with setAPIKey(<yourkeygoeshere>)")
  
  url <- paste0(BASE_URL, path)
  METHOD <- getFromNamespace(method, ns = 'httr')
    
  res <- tryCatch(
    
    METHOD(url,
           query = params,
           body,
           add_headers(`X-CoinAPI-Key` = apiKey)
    ),
    
    error = function(e) e
    
  )
  
  parseJSONResponse(res)
  
}

setApiKey <- function(apiKey) {
  
  Sys.setenv(COIN_API_KEY = apiKey)
  
}

parseJSONResponse <- function(res) {

  resText <- content(res, as = "text")
  
  fromJSON(resText, flatten = TRUE)
  
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