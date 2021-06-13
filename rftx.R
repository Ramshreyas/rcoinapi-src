library(httr)
library(digest)
library(caTools)

BASE_URL <- "ftx.com/api"


ftxRequest <- function(credentials, method, path, params = NULL, body = NULL, retries = 0) {
  
  BASE_URL <- "https://ftx.com/api"
  
  if(!validateCredentials(creds)) {
    
    "Error - credentials not set/invalid. Use setCredentials to set your API-key, secret and subaccount(optional)"
    
  } else {
  
    url <- paste0(BASE_URL, path)
    METHOD <- getFromNamespace(method, ns = 'httr')
      
    res <- tryCatch(
      
      METHOD(url,
             query = params,
             body
      ),
      
      error = function(e) e
      
    )
    
    res

  }
}

