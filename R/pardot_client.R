#' Make a call to the Pardot API and return XML
#'
#' @param object A string containing a Pardot Object
#' @param operator A string containing a Pardot Operator
#' @param identifier_field A string with an optional identifier field. Can be null
#' @param identifier A string with an optional identifier that can be null if identifier_field is null
#' @examples
#' \dontrun{
#' set_credentials("your-username", "your-password", "your-user-key")
#' pardot_client("campaign", "query")}
#' @export
#' pardot_client
#' @import httr
#' @import xml2
#' @import XML
#' @import jsonlite

pardot_client <- function(object,operator,identifier_field=NULL,identifier=NULL) {
  # object & operator are required fields
  # identifier fields / identifier are optional
  # optional field to implement <- api_request_params,"&format=",api_format
  param_list <- (as.list(match.call()))

  if (!exists('api_key')) {
    pardot_client.authenticate()
  } else if (exists('api_key') && api_key == "Login failed" ) {
    pardot_client.authenticate()
  } else {
    request_url <- pardot_client.build_url(param_list)
    pardot_client.api_call(request_url)
  }
}


pardot_client.authenticate <- function() {
  # body params must be set in list. Add .env get that will fetch these items
  auth_body  <- list(email = .paRdotEnv$data$pardot_username,
                     password = .paRdotEnv$data$pardot_password,
                     user_key = .paRdotEnv$data$pardot_user_key)

  # make initial API call to authenticate
  fetch_api_call <- POST("https://pi.pardot.com/api/login/version/3", body= auth_body)

  # returns xml node with <api_key>
  api_key <<- xml_text(content(fetch_api_call))
}

pardot_client.api_call <- function(request_url) {
  resp <- GET(request_url)

  if ( resp$status != 200 ) {
    pardot_client.authenticate()
    resp <- GET(request_url, content_type_xml())
  }

  xml_response <- xmlNode(content(resp, "parsed"))
  return(xml_response)
}


pardot_client.get_data_frame <- function(data) {
  jsonResponse <- fromJSON(data)
  d <- as.data.frame(jsonResponse[2])
  return(d)
}

pardot_client.build_url <- function(param_list) {
  # required fields
  api_object = param_list$object
  api_operator = param_list$operator

  # optional fields
  api_identifier_field = pardot_client.scrub_opts(param_list$identifier_field)
  api_identifier = pardot_client.scrub_opts(param_list$identifier)

  request_url <- paste0("https://pi.pardot.com/api/",api_object,"/version/3/do/",api_operator,api_identifier_field,api_identifier,"?api_key=",api_key,"&user_key=",Sys.getenv("PARDOT_USER_KEY"),"&output=bulk&format=json")
  return(request_url)
}

pardot_client.iterative_request_options <- function(currentOptions, theDate) {
  theDate <- gsub(' ', 'T', theDate)
  currentOptions[['created_after']] = theDate
  iterative_request_options <- currentOptions
}

pardot_client.iterative_request_url <- function(requestUrl, theDate) {
  theDate <- gsub(' ', 'T', theDate)
  iterative_request_url <- paste0(requestUrl,"&created_after=",theDate,"&sort_by=created_at&sort_order=ascending")
}

pardot_client.scrub_opts <- function(opt) {
  if( is.null(opt) || opt == '' ) {
    return('/')
  } else {
    new_opt <- paste0('/',opt)
    return(new_opt)
  }
}
