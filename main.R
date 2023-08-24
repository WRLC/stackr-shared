library(httr)
library(jsonlite)
library(base64enc)

# Functions for getting OCLC holdings
wc_get_token <- function() {
  cat("Getting new access token...\n")
  creds <- fromJSON('./wcapi.json')
  client_id <- creds$wcapi$client_id
  client_secret <- creds$wcapi$client_secret

  auth <- base64encode(charToRaw(paste0(client_id, ":", client_secret)))

  tryCatch(
          token <- POST('https://oauth.oclc.org/token',
            body = list(grant_type="client_credentials", scope="wcapi refresh_token"),
            encode = "form",
            add_headers(Authorization=paste("Basic", auth))
          ), error = function(e) {
            print(e)
            return(NA)
          }
  )

  myfile <- data.frame(content(token))
  write(toJSON(myfile), file = "./wcapi_token.json")
  cat("New access token obtained...\n")
}

refresh_wc_token <- function(old_token) {
  cat("Refreshing access token...\n")
  refresh_token <- old_token$refresh_token

  creds <- fromJSON('wcapi.json')
  client_id <- creds$wcapi$client_id
  client_secret <- creds$wcapi$client_secret

  auth <- base64encode(charToRaw(paste0(client_id, ":", client_secret)))

  tryCatch(
          token <- POST('https://oauth.oclc.org/token',
            body = list(refresh_token=refresh_token, grant_type="refresh_token"),
            encode = "form",
            add_headers(Authorization=paste("Basic", auth))
          ), error = function(e) {
            print(e)
            return(NA)
          }
  )
  myfile <- data.frame(content(token))
  write(toJSON(myfile), file = "./wcapi_token.json")
  cat("Access token refreshed...\n")
}