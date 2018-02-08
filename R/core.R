
#Class AdaptixConnection
setClass("AdaptixConnection", representation(apiURL = "character",
                                             apikey = "character",
                                             users.apiURL = "character",
                                             streams.apiURL = "character",
                                             devices.apiURL = "character",
                                             search.apiURL = "character",
                                             forecast.apiURL = "character",
                                             segmentation.apiURL = "character",
                                             configurations.apiURL = "character"))


#' #' Creates an Adaptix Connection object to be supplied to API calls.
#' #'
#' #' @param url the base URL of the API.
#' #' @param api.key Your Adaptix API key.
#' #' @return The Connection object (list).
#' #' @examples
#' #' AdaptixConnect(url = "https://alpha.adaptix.io/api", api.key = "123456789abcdefgh")
#' AdaptixConnect <- function(url, api.key) {
#'   return(list(
#'     apiURL = url,
#'     apikey = api.key,
#'     users.apiURL = paste0(url, "/users/"),
#'     streams.apiURL = paste0(url, "/streams/"),
#'     forecast.apiURL = paste0(url, "/forecasts"),
#'     search.apiURL = paste0(url, "/matches"),
#'     devices.apiURL = paste0(url, "/devices/"),
#'     cluster.apiURL = paste0(url, "/segmentations/"),
#'     configurations.apiURL = paste0(url, "/configurations/")
#'   ))
#' }
#'
#'

#' Creates an Adaptix Connection object to be supplied to API calls.
#'
#' @param url the base URL of the API.
#' @param api.key Your Adaptix API key.
#' @return The Connection object (list).
#' @examples
#' AdaptixConnect(url = "https://alpha.adaptix.io/api", api.key = "123456789abcdefgh")
AdaptixConnect <- function(url, api.key) {
  return(new(Class = "AdaptixConnection", apiURL = url,
             apikey = api.key,
             users.apiURL = paste0(url, "/users/"),
             streams.apiURL = paste0(url, "/streams/"),
             forecast.apiURL = paste0(url, "/forecasts"),
             search.apiURL = paste0(url, "/matches"),
             devices.apiURL = paste0(url, "/devices/"),
             segmentation.apiURL = paste0(url, "/segmentations/"),
             configurations.apiURL = paste0(url, "/configurations/")))
}



#' Basic http POST request to Adaptix.
#'
#' @param conn a valid Adaptix connection object.
#' @param url the complete URL on which to perform the HTTP operation.
#' @param payload payload of the request, supplied either as a JSON \code{character} string or a \code{list}.
#' @param verbose display HTTP operation details.
#' @return the complete \code{httr} \code{response}.
#' @examples
#' AdaptixPostHTTPRequest(conn = conn, url = "https://alpha.adaptix.io/api/streams", payload =  list(name = "my_stream"), verbose = TRUE)
AdaptixPostHTTPRequest <- function(conn, url, payload, verbose = F) {
  if(is.null(payload))
    stop("missing request payload.")

  if(class(payload) == "list") {
    json_payload <- rjson::toJSON(payload)
  } else if(class(payload) == "character") {
    json_payload <- payload
  } else {
    stop("payload is neither of type 'list' or 'character'.")
  }

  if(verbose) {
    r <- httr::POST(url = url,
                    config = httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                               Accept = "*/*"),
                    httr::content_type_json(),
                    body = json_payload,
                    encode = "json", httr::verbose())
  } else {
    r <- httr::POST(url = url,
                    config = httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                               Accept = "*/*"),
                    httr::content_type_json(),
                    body = json_payload,
                    encode = "json")
  }
  return(r)
}



#' Basic http PUT request to Adaptix.
#'
#' @param conn a valid Adaptix connection object.
#' @param url the complete URL on which to perform the HTTP operation.
#' @param payload payload of the request, supplied either as a JSON \code{character} string or a \code{list}.
#' @param verbose display HTTP operation details.
#' @return the complete \code{httr} \code{response}.
#' @examples
#' AdaptixPutHTTPRequest(conn = conn, url = "https://alpha.adaptix.io/api/streams", payload =  list(name = "my_stream"), verbose = TRUE)
AdaptixPutHTTPRequest <- function(conn, url, payload, verbose = F) {
  if(is.null(payload))
    stop("missing request payload.")

  if(class(payload) == "list") {
    json_payload <- rjson::toJSON(payload)
  } else if(class(payload) == "character") {
    json_payload <- payload
  } else {
    stop("payload is neither of type 'list' or 'character'.")
  }

  if(verbose) {
    r <- httr::PUT(url = url,
                   config = httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                              Accept = "*/*"),
                   httr::content_type_json(),
                   body = json_payload,
                   encode = "json",
                   httr::verbose())
  } else {
    r <- httr::PUT(url = url,
                   config = httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                              Accept = "*/*"),
                   httr::content_type_json(),
                   body = json_payload,
                   encode = "json")
  }
  return(r)
}



#' Basic http GET request to Adaptix.
#'
#' @param conn a valid Adaptix connection object.
#' @param url the complete URL on which to perform the HTTP operation.
#' @param verbose display HTTP operation details.
#' @return the complete \code{httr} \code{response}.
#' @examples
#' AdaptixGetHTTPRequest(conn = conn, url = "https://alpha.adaptix.io/api/streams", verbose = TRUE)
AdaptixGetHTTPRequest <- function(conn, url, verbose = F) {
  if(verbose) {
    r <- httr::GET(url,
                   httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                     Accept = "application/json, text/javascript, */*; q=0.01"),
                   httr::verbose())
  } else {
    r <- httr::GET(url = url,
                   config = httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                              Accept = "application/json, text/javascript, */*; q=0.01"))
  }
  return(r)
}


#' Basic http DELETE request to Adaptix.
#'
#' @param conn a valid Adaptix connection object.
#' @param url the complete URL on which to perform the HTTP operation.
#' @param verbose display HTTP operation details.
#' @return the complete \code{httr} \code{response}.
#' @examples
#' AdaptixDeleteHTTPRequest(conn = conn, url = "https://alpha.adaptix.io/api/streams/123456abcdef", verbose = TRUE)
AdaptixDeleteHTTPRequest <- function(conn, url, verbose = F) {
  if(verbose) {
    r <- httr::DELETE(url = url,
                      config = httr::add_headers("X-API-Key" = as.character(conn@apikey),
                                                 Accept = "application/json, text/javascript, */*; q=0.01"),
                      httr::verbose())
  } else {
    r <- httr::DELETE(url = url,
                      config = add_headers("X-API-Key" = as.character(conn@apikey),
                                           Accept = "application/json, text/javascript, */*; q=0.01"))
  }
  return(r)
}







#' Converts a date to a formatted ISO8601 date
#'
#' @param date a date, either as \code{character} string or \code{POSIXct} object.
#' @param tz the timezone of the date (relevant if only when date is \code{POSIXct}).
#' @return an ISO8601 formatted date character string .
#' @examples
#' ConvertDateToISO8601(date = as.POSIXct("2017-01-01"), tz = "GMT")
#' ConvertDateToISO8601(date = "2017-01-01")
ConvertDateToISO8601 <- function(date, tz = "GMT")
{
  date <- strftime(date, format = "%Y-%m-%dT%H:%M:%S.000Z", usetz = F, tz = tz)
  return(date)
}


#' Converts a formatted ISO8601 date to a POSIXct.
#'
#' @param iso8601 a ISO8601 date (please note: ISO8601 dates are GMT).
#' @param tz the timezone in which the output date should be issued.
#' @return an ISO8601 formatted date character string .
#' @examples
#' ConvertISO8601ToDate(iso8601 = "2016-12-31T23:00:00.000Z", tz = "GMT")
ConvertISO8601ToDate <- function(iso8601, tz = "GMT")
{
  date <- as.POSIXct(iso8601, tz = "GMT", format = "%Y-%m-%dT%H:%M:%OSZ")
  attr(date, "tzone") <- tz
  return(date)
}



#' A utility function to convert "span strings"
#'
#' @param span a time span string in the form of [amount|unit]. e.g. "1h", "35m", "3d"...
#' @return the number of seconds corresponding to the supplied time span string
#' @examples
#' ConvertSpanStringToSeconds(span = "2h")
ConvertSpanStringToSeconds <- function(span) {
  t <- gsub("[^[:digit:]]", "", span)
  s <- gsub("[[:digit:]]", "", span)

  t <- as.numeric(t)
  if(s == "m")
    return(t*60)
  else if(s == "h")
    return(t*3600)
  else if(s == "s")
    return(t*1)
  else if(s == "d")
    return(t*86400)
  else if(s == "w")
    return(t*604800)
}



AdaptixCheckRequest <- function(request, valid.status.codes)
{
  if(match(httr::status_code(request), valid.status.codes, nomatch = 0) == 0)
    stop(paste0("Request failed with status code: ", as.character(httr::status_code(request))))
}








