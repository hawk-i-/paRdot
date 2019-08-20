#' Make a call to the Pardot API and return a dataframe listing all prospects
#'
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' pardot_prospects()}
#' @export
#' pardot_prospects
#' @import httr
#' @import xml2
#' @import XML
#' @import jsonlite

pardot_prospects <- function() {
  
  if (!exists('api_key')) {
    pardot_client.authenticate()
  } else if (exists('api_key') && api_key == "Login failed" ) {
    pardot_client.authenticate()
  } else {
    pardot_prospects.api_call()
  }
}

pardot_prospects.api_call <- function() {
  request_body <- list(api_key = api_key,
                       user_key = Sys.getenv("PARDOT_USER_KEY"),
                       output = 'bulk',
                       format = 'json',
                       sort_by = 'created_at',
                       sort_order = 'ascending'
  )
  request_url <- 'https://pi.pardot.com/api/prospect/version/3/do/query'
  resp <- POST(request_url, body = request_body)
  
  if ( resp$status != 200 ) {
    pardot_client.authenticate()
    resp <- POST(request_url, body = request_body)
  }
  
  resp_body <- content(resp, as = 'text', encoding = 'UTF-8')
  raw_df <- pardot_client.get_data_frame(resp_body)
  raw_df$result.prospect.campaign_name <- raw_df$result.prospect.campaign$name
  raw_df <- subset(raw_df, select=-result.prospect.campaign)
  lowest_date <- tail(raw_df, 1)$result.prospect.created_at
  polished_df <- rbind(raw_df)
  
  while (!nrow(raw_df) < 200) {
    print(paste0("Pulling data from", lowest_date))
    loop_body <- pardot_client.iterative_request_options(request_body, lowest_date)
    loop_resp <- POST(request_url, body = loop_body)
    loop_resp_body <- content(loop_resp, type = 'text', encoding = 'UTF-8')
    raw_df <- pardot_client.get_data_frame(loop_resp_body)
    lowest_date <- tail(raw_df, 1)$result.prospect.created_at
    raw_df$result.prospect.campaign_name <- raw_df$result.prospect.campaign$name
    raw_df <- subset(raw_df, select=-result.prospect.campaign)
    polished_df <- rbind(polished_df, raw_df)
  }
  
  return(polished_df)
}


