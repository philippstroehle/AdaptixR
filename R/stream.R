

#' List all Adaptix streams.
#'
#' @param conn a valid Adaptix connection object.
#' @param as.data.frame if TRUE, returns result as a data.frame object, otherwise as a list.
#' @param expand wether or not to request the full (expanded) information of the requested resource(s) - see Adaptix API documentation.
#' @param verbose display HTTP operation details.
#' @return a listings of streams.
#' @examples
#' AdaptixGetStreams(conn = conn, verbose = F)
AdaptixGetStreams <- function(conn, as.data.frame = T, expand = F, verbose = FALSE) {
  out <- tryCatch({
    items <- list()
    if(expand)
      url <- paste(conn@streams.apiURL, "expand=true", sep = "?")
    else url <- conn@streams.apiURL
    r <- AdaptixGetHTTPRequest(conn = conn, url = url, verbose = verbose)
    AdaptixCheckRequest(r$status_code, "200")
    items <- httr::content(r)$items

    while(httr::content(r)$limit < content(r)$count) {
      if(expand)
        url <- paste(httr::content(r)$`next`, "expand=true", sep = "&")
      else
        url <- httr::content(r)$`next`
      r <- AdaptixGetHTTPRequest(conn = conn, url, verbose)
      AdaptixCheckRequest(request = r, "200")
      items <- c(items, httr::content(r)$items)
    }
    if(as.data.frame)
      df <- data.frame(matrix(unlist(items), nrow=length(items), byrow = T), stringsAsFactors = FALSE)
    else
      df <- items
    return(df)
  }, error = function(cond) {
    warning(cond)
    return(NULL)
  })
  return(out)
}





#' List all Adaptix streams, with expanded information.
#'
#' @param conn a valid Adaptix connection object.
#' @param verbose display HTTP operation details.
#' @return a listing of streams, in list format.
#' @examples
#' AdaptixGetStreamsExpanded(conn = conn, verbose = F)
AdaptixGetStreamsExpanded <- function(conn, verbose = FALSE) {
  items <- list()
  r <- AdaptixGetHTTPRequest(conn = conn, url = paste0(conn@streams.apiURL, "?expand=true"), verbose = verbose)
  AdaptixCheckRequest(request = r, "200")
  items <- httr::content(r)$items


  while(httr::content(r)$limit < httr::content(r)$count) {
    r <- AdaptixGetHTTPRequest(conn = conn, url = paste0(httr::content(r)$`next`, "&expand=true"), verbose = verbose)
    AdaptixCheckRequest(request = r, "200")
    items <- c(items, httr::content(r)$items)
  }
  return(items)
}





#' Get a dataframe of all Adaptix streams as a data frame with stream id, name, and href columns
#'
#' @param conn a valid Adaptix connection object.
#' @param verbose display HTTP operation details.
#' @return a listing of streams, in a dataframe format.
#' @examples
#' AdaptixGetStreamsWithNames(conn = conn, verbose = F)
AdaptixGetStreamsWithNames <- function(conn, verbose = FALSE) {
  items <- AdaptixGetStreamsExpanded(conn = conn, verbose = verbose)
  if(length(items) == 0)
    return(NULL)
  names <- list()
  for(i in 1:length(items)) {
    items[[i]] <- items[[i]][c("id","name", "href")]
  }
  df <- data.frame(matrix(unlist(items), nrow=length(items), byrow=T), stringsAsFactors = FALSE)
  colnames(df) <- c("id","name", "href")
  return(df)
}





#' Get a vector of all Adaptix stream ids
#'
#' @param conn a valid Adaptix connection object.
#' @param verbose display HTTP operation details.
#' @return a vector of streams ids
#' @examples
#' AdaptixGetAllStreamIds(conn = conn, verbose = F)
AdaptixGetAllStreamIds <- function(conn, verbose = FALSE) {
  df <- AdaptixGetStreams(conn = conn, verbose = verbose)
  df <- sapply(strsplit(df$X1, "/"), tail, 1)
  return(df)
}




#' Get a specific Adaptix stream
#'
#' @param conn a valid Adaptix connection object.
#' @param streamURL the location of the stream, as a URL (as provided in the "href" field of API responses).
#' @param streamID  the stream id (ignored if streamURL is provided)
#' @param verbose display HTTP operation details.
#' @return the stream information, as a list
#' @examples
#' AdaptixGetStream(conn = conn, streamID = "123456abcdef", verbose = F)
#' AdaptixGetStream(conn = conn, streamURL = "https://alpha.adaptix.io/api/streams/123456abcdef", verbose = F)
AdaptixGetStream <- function(conn, streamURL = NULL, streamID = NULL, verbose = FALSE) {
  if(is.null(streamURL) && is.null(streamID))
    stop("invalid parameters.")
  if(!is.null(streamURL)) {
    r <- AdaptixGetHTTPRequest(conn = conn, url = streamURL, verbose = verbose)
  } else if(!is.null(streamID)){
    r <- AdaptixGetHTTPRequest(conn = conn, url = paste0(conn@streams.apiURL, streamID), verbose = verbose)
  }
  return(httr::content(r))
}




#' Create an Adaptix stream
#'
#' @param conn a valid Adaptix connection object.
#' @param name the name of the stream
#' @param device  if not NULL, associates the stream with a device.
#' @param description the description of the stream
#' @param units if not NULL, documents the unit ofor this stream's data.
#' @param anomaly.monitoring [REQUIRES ANOMALY DETECTION APP TO BE ENABLED] Enable anomaly detection on this stream.
#' @param store.points wether or not to store the points
#' @param data.rate if not NULL, indicates the normal data rate for the points of this stream.
#' @param verbose display HTTP operation details.
#' @return the newly created stream information, as a list
#' @examples
#' AdaptixCreateStream(conn = conn, name = "my_stream", verbose = F)
AdaptixCreateStream <- function(conn, name, device = NULL, description = NULL, units = NULL, anomaly.monitoring = F, store.points = T, data.rate = NULL, verbose = FALSE) {
  if(is.null(name))
    stop("no id provided, but required...")

  payload <- list(name = name,
                  anomaly_monitoring = anomaly.monitoring,
                  store_points = store.points)
  if(!is.null(description))
    payload$description <- description
  if(!is.null(units))
    payload$units <- units
  if(!is.null(device))
    payload$device_id <- device
  if(!is.null(data.rate))
    payload$data_rate <- data.rate

  r <- AdaptixPostHTTPRequest(conn = conn, conn@streams.apiURL, payload = payload, verbose = verbose)
  AdaptixCheckRequest(request = r, "201")
  return(httr::content(r))
}





#' Delete an Adaptix stream
#'
#' @param conn a valid Adaptix connection object.
#' @param streamURL the location of the stream, as a URL (as provided in the "href" field of API responses).
#' @param streamID  the stream id (ignored if streamURL is provided)
#' @param verbose display HTTP operation details.
#' @return the information of the stream that was just deleted, as a list
#' @examples
#' AdaptixCreateStream(conn = conn, streamID = "123456abcdef", verbose = F)
AdaptixDeleteStream <- function(conn, streamURL = NULL, streamID = NULL, verbose = FALSE) {
  if(is.null(streamURL) && is.null(streamID))
    stop("invalid parameters.")
  if(!is.null(streamURL)) {
    r <- AdaptixDeleteHTTPRequest(conn = conn, url = streamURL, verbose = verbose)
  } else if(!is.null(streamID)){
    r <- AdaptixDeleteHTTPRequest(conn = conn, url = paste0(conn@streams.apiURL, streamID), verbose = verbose)
  }
  AdaptixCheckRequest(request = r, "202")
  return(httr::content(r))
}



#' Delete the points of an Adaptix stream
#'
#' @param conn a valid Adaptix connection object.
#' @param streamURL the location of the stream, as a URL (as provided in the "href" field of API responses).
#' @param streamID  the stream id (ignored if streamURL is provided)
#' @param verbose display HTTP operation details.
#' @return the collection of points that were just deleted, as a list
#' @examples
#' AdaptixCreateStream(conn = conn, streamID = "123456abcdef", verbose = F)
AdaptixDeleteStreamPoints <- function(conn, streamURL = NULL, streamID = NULL, verbose = FALSE) {
  if(is.null(streamURL) && is.null(streamID))
    stop("invalid parameters.")
  if(!is.null(streamURL)) {
    url <- paste(streamURL, "/points")
  } else if(!is.null(streamID)){
    url <- paste0(conn@streams.apiURL, streamID, "/points")
  }
  AdaptixCheckRequest(request = r, "202")
  return(httr::content(r))
}



#' Update an Adaptix stream
#'
#' @param conn a valid Adaptix connection object.
#' @param streamURL the location of the stream, as a URL (as provided in the "href" field of API responses).
#' @param streamID  the stream id (ignored if streamURL is provided)
#' @param name the name of the stream.
#' @param description the name of the stream.
#' @param device  if not NULL, associates the stream with a device.
#' @param units if not NULL, documents the unit ofor this stream's data.
#' @param anomaly.monitoring [REQUIRES ANOMALY DETECTION APP TO BE ENABLED] Enable anomaly detection on this stream.
#' @param store.points wether or not to store the points
#' @param data.rate if not NULL, indicates the normal data rate for the points of this stream.
#' @param verbose display HTTP operation details.
#' @return the newly created stream information, as a list
#' @examples
#' AdaptixUpdateStream(conn = conn, name = "my_stream", verbose = F)
AdaptixUpdateStream <- function(conn, streamURL = NULL, streamID = NULL, name = NULL, device = NULL, description = NULL, units = NULL, anomaly.monitoring = F, store.points = T, data.rate = NULL, verbose = FALSE) {
  if(is.null(streamURL) && is.null(streamID))
    stop("invalid parameters.")

  payload <- list(anomaly_monitoring = anomaly.monitoring,
                  store_points = store.points)

  if(!is.null(name))
    payload$name <- name
  if(!is.null(description))
    payload$description <- description
  if(!is.null(units))
    payload$units <- units
  if(!is.null(device))
    payload$device_id <- device
  if(!is.null(data.rate))
    payload$data_rate <- data.rate


  if(!is.null(streamURL)) {
    r <- AdaptixPutHTTPRequest(conn = conn, url = streamURL, payload = payload, verbose = verbose)
  } else if(!is.null(streamID)){
    r <- AdaptixPutHTTPRequest(conn = conn, url = paste0(conn@streams.apiURL, streamID), payload = payload, verbose = verbose)
  }
  return(ifelse(status_code(r) == 200, T, F))
}







