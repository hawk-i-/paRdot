#' Make a call to the Pardot API and return a dataframe listing all campaigns
#'
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' pardot_campaigns()}
#' @export
#' pardot_campaigns
#' @import httr
#' @import xml2
#' @import XML
#' @import jsonlite

pardot_campaigns <- function() {

  if (!exists('api_key')) {
    pardot_client.authenticate()
  } else if (exists('api_key') && api_key == "Login failed" ) {
    pardot_client.authenticate()
  } else {
    pardot_campaigns.api_call()
  }
}

pardot_campaigns.api_call <- function() {
  request_body  <- list(api_key = api_key,
                     user_key = Sys.getenv("PARDOT_USER_KEY"),
                     output = 'bulk',
                     format = 'json',
                     sort_by = 'id',
                     sort_order = 'ascending'
                     )
  
  
  request_url <- 'https://pi.pardot.com/api/campaign/version/3/do/query'
  resp <- POST(request_url, body = request_body)

  if ( resp$status != 200 ) {
    pardot_client.authenticate()
    resp <- POST(request_url, body = request_body)
  }
  
  resp_body <- content(resp, as = "text", encoding = 'UTF-8')
  raw_df <- pardot_client.get_data_frame(resp_body)
  lowest_id <- tail(raw_df, 1)$result.campaign.id
  polished_df <- rbind(raw_df)

  while (!nrow(raw_df) < 200) {
    print(paste0("Pulling data from low ID", lowest_id))
    request_body[['id_greater_than']] = lowest_id
    loop_resp <- POST(request_url, body = request_body)
    loop_resp_body = content(loop_resp, as = 'text', encoding = 'UTF-8')
    raw_df <- pardot_client.get_data_frame(loop_resp_body)
    lowest_id <- tail(raw_df, 1)$result.campaign.id

    polished_df <- rbind(polished_df, raw_df)
  }

  return(polished_df)
}
