library(httr)
library(digest)

BASE_URL <- "ftx.com/api"
CREDENTIALS <- c(FTX_KEY = "",
                 FTX_SECRET = "",
                 FTX_SUBACCOUNT = "")

SUBACCOUNT_ENDPOINT <- "/subaccounts"



getAllSubaccounts <- function(request, credentials) {
  
}

ftxRequest <- function(credentials = list(), method, path, params = NULL, body = NULL, retries = 0) {
  
  BASE_URL <- "https://ftx.com/api"
  
  if(!validateCredentials(creds)) {
    
    "Error - credentials not set/invalid. Use setCredentials to set your API-key and secret"
    
  } else {
  
    headers <- setHeaders(credentials, method, path, body)
    
    METHOD <- getFromNamespace(method, ns = 'httr')
      
    res <- tryCatch(
      
      METHOD(BASE_URL,
             path = path,
             add_headers(.headers = headers),
             query = params,
             body
      ),
      
      error = function(e) e
      
    )
    
    res

  }
}

setHeaders <- function(credentials,
                       method,
                       path,
                       body = NULL) {
  
  BASE_URL <- "ftx.com/api"
  
  ftx_ts <- timestamp()
  signaturePayload <- paste0(ftx_ts, method, path, body)
  ftx_sign <- hmac(credentials$FTX_SECRET, signaturePayload, "sha256")
  
  c("FTX-KEY" = credentials$FTX_KEY,
    "FTX-SIGN" = ftx_sign,
    "FTX-TS" = ftx_ts,
    "FTX-SUBACCOUNT" = credentials$FTX_SUBACCOUNT)
}

validateCredentials <- function(credentials = list()) {
  
  TRUE
  
}

timestamp <- function() {
  as.character(round(as.numeric(Sys.time()) * 1e3))
}
