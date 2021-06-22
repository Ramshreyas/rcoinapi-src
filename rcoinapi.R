library(httr)
library(jsonlite)
library(lubridate)
library(xts)

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

getCryptoSymbols <- function(symbolId = NULL, 
                             exchangeId = NULL, 
                             assetId = NULL) {
  
  if(is.list(symbolId) & is.list(exchangeId) & is.list(assetId)) {
    
    endpoint <- SYMBOLS_ENDPOINT
    
    executeRequest("GET", endpoint, params = list("filter_symbol_id" = listToStringArray(symbolId), "filter_exchange_id" = listToStringArray(exchangeId), "filter_asset_id" = listToStringArray(assetId)))
    
  } else {
    
    endpoint <- paste0(SYMBOLS_ENDPOINT, exchangeId)
    
    executeRequest("GET", endpoint)
    
  }
  
}

#----------------ExchangeRates---------------------------------------------------------------------------------------------

getExchangeRate <- function(assetIdBase, 
                            assetIdQuote, 
                            time = NULL) {
  
  endpoint <- paste0(EXCHANGERATE_ENDPOINT, assetIdBase, "/", assetIdQuote)
  
  if(is.null(time)) {
    
    executeRequest("GET", endpoint)
    
  } else {
    
    executeRequest("GET", endpoint, params = list("time" = time))
    
  }
  
}

getAllExchangeRates <- function(assetIdBase, 
                                invert = FALSE, 
                                filter_asset_id = NULL) {
  
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
                                       limit = 100) {
  
  symbolName <- paste0(assetIdBase, "/", assetIdQuote)
  
  endpoint <- paste0(EXCHANGERATE_ENDPOINT, assetIdBase, "/", assetIdQuote, "/history")
    
  xtsData <- executeXtsRequest("GET", endpoint, params = list("period_id" = periodId, "time_start" = timeStart, "time_end" = timeEnd, "limit" = as.character(limit)))
  
  renameOHLCVColumns(xtsData, symbolName)
  
}

#----------------OHLCV---------------------------------------------------------------------------------------------

getOHLCVPeriods <- function() {
  
  endpoint <- paste0(OHLCV_ENDPOINT, "periods")
  
  executeRequest("GET", endpoint)
  
}

getLatestOHLCV <- function(assetIdBase,
                           assetIdQuote = NULL,
                           periodId,
                           includeEmptyItems = FALSE,
                           limit = 100) {
  
  if(is.null(assetIdQuote)) {
    
    symbolName <- assetIdBase
    
    endpoint <- paste0(OHLCV_ENDPOINT, assetIdBase, "/latest")
    
  } else {
    
    symbolName <- paste0(assetIdBase, "/", assetIdQuote)
    
    endpoint <- paste0(OHLCV_ENDPOINT, assetIdBase, "/", assetIdQuote, "/latest")
    
  }
  
  xtsData <- executeXtsRequest("GET", endpoint, params = list("period_id" = periodId, "limit" = as.character(limit), "include_empty_items" = includeEmptyItems))
  
  renameOHLCVColumns(xtsData, symbolName)
  
}

getHistoricalOHLCV <- function(assetIdBase,
                               assetIdQuote = NULL,
                               periodId,
                               timeStart, 
                               timeEnd = NULL,
                               includeEmptyItems = FALSE,
                               limit = 100) {
  
  if(is.null(assetIdQuote)) {
    
    symbolName <- assetIdBase
    
    endpoint <- paste0(OHLCV_ENDPOINT, assetIdBase, "/latest")
    
  } else {
    
    symbolName <- paste0(assetIdBase, "/", assetIdQuote)
    
    endpoint <- paste0(OHLCV_ENDPOINT, assetIdBase, "/", assetIdQuote, "/latest")
    
  }
  
  if(is.null(timeEnd)) {
  
    xtsData <- executeXtsRequest("GET", endpoint, params = list("period_id" = periodId, "time_start" = timeStart, "limit" = as.character(limit), "include_empty_items" = includeEmptyItems))
    
  } else {
    
    xtsDate <- executeXtsRequest("GET", endpoint, params = list("period_id" = periodId, "time_start" = timeStart, "time_end" = timeEnd, "limit" = as.character(limit), "include_empty_items" = includeEmptyItems))
    
  }
  
  renameOHLCVColumns(xtsData, symbolName)
  
}

#----------------TRADES---------------------------------------------------------------------------------------------

getTrades <- function(symbol = NULL, 
                      limit = 100, 
                      filterSymbol = NULL, 
                      include = FALSE) {
  
  if(is.null(symbol)) {
    
    endpoint <- paste0(TRADES_ENDPOINT, "latest")
    
    if(is.null(filterSymbol)) {
      
      p <- list("limit" = limit)      
      
    } else {
      
      p <- list("limit" = limit, "filter_symbol_id" = filterSymbol)
      
    }
    
    
  } else {
    
    endpoint <- paste0(TRADES_ENDPOINT, "/", symbol, "/latest")
    
    p <- list("limit" = limit, "include_id" = include)
    
  }
  
  executeRequest("GET", endpoint, params = p)
  
}

getHistoricalTrades <- function(symbol,
                                timeStart, 
                                timeEnd = NULL,
                                limit = 100, 
                                include = FALSE) {
  
  endpoint <- paste0(TRADES_ENDPOINT, symbol, "/history")
    
  p <- list("time_start" = timeStart, "time_end" = timeEnd, "limit" = limit, "include_id" = include)
  
  executeXtsRequest("GET", endpoint, params = p, indexBy = "time_exchange")
  
}

#----------------QUOTES---------------------------------------------------------------------------------------------



#----------------ORDERBOOK---------------------------------------------------------------------------------------------

getOrderbook <- function(symbol, depth = 20) {
  
  endpoint <- paste0(MARKETS_ENDPOINT, "/", symbol, "/orderbook?depth=", depth)
  
  executeRequest("GET", endpoint)
  
}


#----------------ORDERBOOKL3---------------------------------------------------------------------------------------------



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

executeXtsRequest <- function(method, path, params = NULL, body = NULL, retries = 0, indexBy = "time_period_start") {

  res <- executeRequest(method, path, params, body, retries)
  
  xts(res, order.by = as_datetime(res[,indexBy]))
   
}

renameOHLCVColumns <- function(xtsObject, symbolName) {
  
  if(ncol(xtsObject) == 8) {
  
    colnames(xtsObject) <- c("time_period_start",
                             "time_period_end",
                             "time_open",
                             "time_close",
                             paste0(symbolName, ".Open"),
                             paste0(symbolName, ".High"),
                             paste0(symbolName, ".Low"),
                             paste0(symbolName, ".Close"))
    
  } else {
    
    colnames(xtsObject) <- c("time_period_start",
                             "time_period_end",
                             "time_open",
                             "time_close",
                             paste0(symbolName, ".Open"),
                             paste0(symbolName, ".High"),
                             paste0(symbolName, ".Low"),
                             paste0(symbolName, ".Close"),
                             paste0(symbolName, ".Volume"),
                             "trades_count")
    
  }
  
  xtsObject
  
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