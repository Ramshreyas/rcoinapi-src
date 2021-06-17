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
  
  if(is.list(exchangeId)) {
    
    endpoint <- EXCHANGES_ENDPOINT
    
    executeRequest("GET", endpoint, params = list("filter_exchange_id" = listToStringArray(exchangeId)))
    
  } else {
    
    endpoint <- paste0(EXCHANGES_ENDPOINT, exchangeId)
    
    executeRequest("GET", endpoint)
    
  }
  
}

getExchangeIcons <- function(sizeText) {
  
  endpoint <- paste0(EXCHANGES_ENDPOINT, "icons/", sizeText)
  
  executeRequest("GET", endpoint)
  
}

getAssets <- function(assetId = NULL) {
  
  if(is.list(assetId)) {
    
    endpoint <- ASSETS_ENDPOINT
    
    executeRequest("GET", endpoint, params = list("filter_asset_id" = listToStringArray(assetId)))
    
  } else {
    
    endpoint <- paste0(ASSETS_ENDPOINT, assetId)
    
    executeRequest("GET", endpoint)
    
  }
  
}

getAssetIcons <- function(sizeText) {
  
  endpoint <- paste0(ASSETS_ENDPOINT, "icons/", sizeText)
  
  executeRequest("GET", endpoint)
  
}

getSymbols <- function(symbolId = NULL, exchangeId = NULL, assetId = NULL) {
  
  if(is.list(symbolId) & is.list(exchangeId) & is.list(assetId)) {
    
    endpoint <- SYMBOLS_ENDPOINT
    
    executeRequest("GET", endpoint, params = list("filter_symbol_id" = listToStringArray(symbolId), "filter_exchange_id" = listToStringArray(exchangeId), "filter_asset_id" = listToStringArray(assetId)))
    
  } else {
    
    endpoint <- paste0(SYMBOLS_ENDPOINT, exchangeId)
    
    executeRequest("GET", endpoint)
    
  }
  
}

getExchangeRate <- function(assetIdBase, assetIdQuote, time = NULL) {
  
  endpoint <- paste0(EXCHANGERATE_ENDPOINT, assetIdBase, "/", assetIdQuote)
  
  if(is.null(time)) {
    
    executeRequest("GET", endpoint)
    
  } else {
    
    executeRequest("GET", endpoint, params = list("time" = time))
    
  }
  
}

getAllExchangeRates <- function(assetIdBase, invert = FALSE, filter_asset_id = NULL) {
  
  endpoint <- paste0(EXCHANGERATE_ENDPOINT, assetIdBase)
  
  if(invert) inv <- "true" else inv <- "false"
  
  if(is.null(filter_asset_id)) {
    
    executeRequest("GET", endpoint, params = list("invert" = inv))
    
  } else {
    
    executeRequest("GET", endpoint, params = list("invert" = inv, "filter_asset_id" = listToStringArray(filter_asset_id)))
    
  }
  
}

getHistoricalExchangeRatePeriods <- function() {
  
  endpoint <- paste0(EXCHANGERATE_ENDPOINT, "history/periods")
  
  executeRequest("GET", endpoint)
  
}

getHistoricalExchangeRates <- function(assetIdBase, 
                                       assetIdQuote, 
                                       periodId,
                                       timeStart, 
                                       timeEnd,
                                       limit = NULL) {
  
  endpoint <- paste0(EXCHANGERATE_ENDPOINT, assetIdBase, "/", assetIdQuote, "/history")
  
  if(is.null(limit)) {
    
    executeRequest("GET", endpoint, params = list("period_id" = periodId, "time_start" = timeStart, "time_end" = timeEnd))
    
  } else {
    
    executeRequest("GET", endpoint, params = list("period_id" = periodId, "time_start" = timeStart, "time_end" = timeEnd, "limit" = limit))
    
  }
  
}

getOHLCVPeriods <- function() {
  
  endpoint <- paste0(OHLCV_ENDPOINT, "periods")
  
  executeRequest("GET", endpoint)
  
}

getTrades <- function(symbol, start_time_millis, end_time_millis) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/trades")
  
  executeRequest("GET", endpoint, params = paginate(start_time_millis, end_time_millis))
  
}

getOrderbook <- function(symbol, depth = 20) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/orderbook?depth=", depth)
  
  executeRequest("GET", endpoint)
  
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
  
  print(res)
  
  parseJSONResponse(res)
  
}

listToStringArray <- function(l) {
  
  stringArray <- ""
  
  for (item in l) {
    stringArray <- paste0(stringArray, item, ",")
  }
  
  substr(stringArray, 1, nchar(stringArray) - 1)
  
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