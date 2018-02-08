

#' Collects the points of an Adaptix stream.
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream from which to retrieve the points.
#' @param from if not NULL, the starting date of the time window.
#' @param to if not NULL, the closing date of the time window.
#' @param group.by if NULL, the raw points will be retrieved. If supplied as a "span" string describing a time window, the points will be retrieved as an aggregation (MEAN by default, for other options see aggregator parameter) over consecutive windows of the specified length - see Adaptix API documentation.
#' @param aggregator if NULL, the type of aggregation that will be processed over the aggregation windows (group.by parameter) - see Adaptix API documentation for details.
#' @param fill if NULL, missing data in the stream points will be filled according to the supplied supported operator
#' @param verbose display HTTP operation details.
#' @return a data.frame with the requested collection of points.
#' @examples
#' AdaptixGetPoints(conn = conn, stream = "123456abcdef", group.by = "1h", aggregator = "MEAN", verbose = F)
AdaptixGetPoints <- function(conn, stream, from = NULL, to = NULL, groupby = NULL, aggregator = NULL, fill = NULL, verbose = FALSE)
{
  if(is.null(stream))
    stop("missing stream parameter")
  requestURL <- paste(conn@streams.apiURL, as.character(stream),
                      "/points",
                      sep = "")
  if(!is.null(from) && !is.null(to)) {
    from <- ConvertDateToISO8601(from)
    to <- ConvertDateToISO8601(to)
    requestURL <- paste(requestURL,
                        "?from=",
                        from,
                        "&to=",
                        to,
                        sep = "")

    if(!is.null(groupby)){
      requestURL <- paste(requestURL,
                          "&groupBy=",
                          as.character(groupby),
                          sep = "")
      if(!is.null(aggregator)) {
        requestURL <- paste(requestURL,
                            "&aggregate=",
                            as.character(aggregator),
                            sep = "")
      }
      if(!is.null(fill)) {
        requestURL <- paste(requestURL,
                            "&fill=",
                            as.character(fill),
                            sep = "")
      }
    }
  } else {
    if(!is.null(groupby)){
      requestURL <- paste(requestURL,
                          "?groupBy=",
                          as.character(groupby),
                          sep = "")
      if(!is.null(aggregator)) {
        requestURL <- paste(requestURL,
                            "&aggregate=",
                            as.character(aggregator),
                            sep = "")
      }
      if(!is.null(fill)) {
        requestURL <- paste(requestURL,
                            "&fill=",
                            as.character(fill),
                            sep = "")
      }
    }
  }

  r <- AdaptixGetHTTPRequest(conn = conn, url = requestURL, verbose)
  AdaptixCheckRequest(request = r, "200")

  if(length(httr::content(r)$items) == 0)
    return(NULL)
  df <- data.frame(matrix(unlist(httr::content(r)$items), nrow=length(httr::content(r)$items), byrow=T), stringsAsFactors = FALSE)
  colnames(df) <- c("value" , "at")
  df$value <- as.numeric(df$value)
  df$at <- as.POSIXct(ConvertISO8601ToDate(df$at), tz = "GMT")
  return(df)
}




#' Collects the last N points of an Adaptix stream.
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream from which to retrieve the points.
#' @param n the number of trailing points to collect.
#' @param verbose display HTTP operation details.
#' @return a data.frame with the requested collection of points.
#' @examples
#' AdaptixGetLastPoints(conn = conn, stream = "123456abcdef", n = 5, verbose = F)
AdaptixGetLastPoints <- function(conn, stream, n = 1, verbose = F)
{
  requestURL <- paste0(conn@streams.apiURL, stream, "/points/last?n=", n)
  r <- AdaptixGetHTTPRequest(conn = conn, url = requestURL, verbose = verbose)
  AdaptixCheckRequest(request = r, "200")
  if(length(httr::content(r)$items) == 0)
    return(NULL)
  df <- data.frame(matrix(unlist(httr::content(r)$items), nrow=length(httr::content(r)$items), byrow=T), stringsAsFactors = FALSE)
  colnames(df) <- c("value" , "at")
  df$value <- as.numeric(df$value)
  df$at <- as.POSIXct(ConvertISO8601ToDate(df$at), tz = "GMT")
  return(df)
}




#' Collects the notifications of an Adaptix stream.
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream from which to retrieve the notifications.
#' @param from if not NULL, the starting date of the time window.
#' @param to if not NULL, the closing date of the time window.
#' @param group.by if NULL, the raw notifications will be retrieved. If supplied as a "span" string describing a time window, the points will be averaged over consecutive windows of the specified length - see Adaptix API documentation.
#' @param verbose display HTTP operation details.
#' @return a data.frame with the requested collection of points.
#' @examples
#' AdaptixGetNotifications(conn = conn, stream = "123456abcdef", group.by = "1h",  verbose = F)
AdaptixGetNotifications <- function(conn, stream, from = NULL, to = NULL, groupby = NULL, verbose = FALSE)
{
  if(is.null(stream))
    stop("missing stream parameter")
  requestURL <- paste(conn@streams.apiURL, as.character(stream),
                      "/notifications",
                      sep = "")
  if(!is.null(from) && !is.null(to)) {
    from <- ConvertDateToISO8601(from)
    to <- ConvertDateToISO8601(to)
    requestURL <- paste(requestURL, as.character(stream),
                        "/notifications?from=",
                        from,
                        "&to=",
                        to,
                        sep = "")
  }

  if(!is.null(groupby)){
    requestURL <- paste(requestURL,
                        "&groupBy=",
                        as.character(groupby),
                        sep = "")
  }

  r <- AdaptixGetHTTPRequest(conn = conn, url = requestURL, verbose = verbose)
  AdaptixCheckRequest(request = r, "200")
  if(length(httr::content(r)$items) == 0)
    return(NULL)
  df <- data.frame(matrix(unlist(httr::content(r)$items), nrow=length(httr::content(r)$items), byrow=T), stringsAsFactors = FALSE)
  colnames(df) <- c("value" , "at", "event")
  df$value <- as.numeric(df$value)
  df$at <- as.POSIXct(ConvertISO8601ToDate(df$at), tz = "GMT")
  df$event <- as.character(df$event)
  return(df)
}




#' Publishes points to an Adaptix stream.
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream from which to retrieve the points.
#' @param points a data frame with "at" and "value" columns containing the points to publish to Adaptix.
#' @param group.by chunk the operation in several requests, each publiching "group.by" points of the supplied data frame.
#' @param verbose display HTTP operation details.
#' @return the URL location of the created points
#' @examples
#' AdaptixPublishPoints(conn = conn, stream = "123456abcdef", points = my_points.df, verbose = F)
AdaptixPublishPoints <- function(conn, stream, points, group.by = 1, verbose = F)
{
  if(is.null(stream) || is.null(points))
    stop("missing parameters.")

  #filter potential NAs
  points <- na.omit(points)
  r <- "ERROR" #dumb init

  i <- 0
  while(i < length(points$at)) {
    l <- list()
    for(j in 1:min(group.by, length(points$at) - i)) {
      d <- list(at = points$at[[i+j]], value = points$value[[i+j]])
      l[[j]] <- d
    }
    i <- i + group.by

    payload <- list(points = l)
    r <- AdaptixPostHTTPRequest(conn = conn,
                               url = paste0(conn@streams.apiURL, stream, "/points"),
                               payload = l, verbose = verbose)
    AdaptixCheckRequest(request = r, c("201", "100"))
  }
  return(r$url)
}

