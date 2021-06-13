library(httr)
library(digest)
library(caTools)

BASE_URL <- "ftx.com/api"
SUBACCOUNT_ENDPOINT <- "/subaccounts"



getAllSubaccounts <- function(credentials) {
  
  ftxRequest(credentials, "GET", "/subaccounts")
  
}

ftxRequest <- function(credentials, method, path, params = NULL, body = NULL, retries = 0) {
  
  BASE_URL <- "https://ftx.com/api"
  
  if(!validateCredentials(creds)) {
    
    "Error - credentials not set/invalid. Use setCredentials to set your API-key, secret and subaccount(optional)"
    
  } else {
  
    headers <- setHeaders(credentials, method, path, body)
    url <- paste0(BASE_URL, path)
    METHOD <- getFromNamespace(method, ns = 'httr')
      
    res <- tryCatch(
      
      METHOD(url,
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
  
  print(signaturePayload)
  
  ftx_sign <- hmac(base64encode(credentials$FTX_SECRET, "character"), signaturePayload, "sha256")
  
  if(is.null(credentials$FTX_SUBACCOUNT)) {

    c("FTX-KEY" = credentials$FTX_KEY,
      "FTX-SIGN" = ftx_sign,
      "FTX-TS" = ftx_ts)
     
  } else {
    
    c("FTX-KEY" = credentials$FTX_KEY,
      "FTX-SIGN" = ftx_sign,
      "FTX-TS" = ftx_ts,
      "FTX-SUBACCOUNT" = credentials$FTX_SUBACCOUNT)
    
  }
}

setCredentials <- function(ftx_api, ftx_secret, ftx_subaccount = NULL) {
  
  if(is.null(ftx_subaccount)) {
    
    list("FTX_API" = ftx_api,
         "FTX_SECRET" = ftx_secret)
    
  } else {
    
    list("FTX_API" = ftx_api,
         "FTX_SECRET" = ftx_secret,
         "FTX_SUBACCOUNT" = ftx_subaccount)  
    
  }
  
}

validateCredentials <- function(credentials = list()) {
  
  TRUE
  
}

timestamp <- function() {
  as.character(round(as.numeric(Sys.time())*1000, digits = 0))
}
