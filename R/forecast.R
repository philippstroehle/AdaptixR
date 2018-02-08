

#' Perform forecast.
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream from which to retrieve the points.
#' @param from if not NULL, the starting date of the forecast. If NULL, defaults to the last point of the stream.
#' @param to if not NULL, the closing date of the forecast. required if span is NULL.
#' @param span a "span string" describing the forecast horizon. required if to is NULL.
#' @param window a "span string" describing a constraint on the data history to consider in the forecast.
#' @param aggregated if TRUE, the forecast will be provided as an aggregated forecast scenario, along with confidence margins.
#' @param individual if TRUE, the forecast will be provided as multiple plausible
#' @param number.of.scenarios if individual parameter is TRUE, restricts the number of scenarios that can be provided. If equal to 0, all scenarios deemed plausible by Adaptix will be provided.
#' @param rate desired frame rate of the forecast scnearios. Currently under development.
#' @param verbose display HTTP operation details.
#' @return a data.frame with the requested collection of points.
#' @examples
#' AdaptixForecast(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", individual = T, number.of.scenarios = 3, verbose = F)
AdaptixForecast <- function(conn, stream, from = NULL, to = NULL, span = NULL, history = NULL, aggregated = FALSE, individual = TRUE, number.of.scenarios = 0, rate = NULL, verbose = FALSE)
{
  #Check minimal parameters
  if(is.null(to) && is.null(span))
    stop("missing parameters [to or span]")

  parameters <- list(stream_id = stream ,
                     individual = individual,
                     aggregated = aggregated,
                     number_of_scenarios = number.of.scenarios)
  if(!is.null(from) && from != "")
    parameters$from <- ConvertDateToISO8601(from)

  if(!is.null(to) && to != "")
    parameters$to <- ConvertDateToISO8601(to)

  if(!is.null(span))
    parameters$span <- span

  if(!is.null(history))
    parameters$window <- history

  if(!is.null(rate))
    parameters$rate <- rate

  #send request
  r <- AdaptixPostHTTPRequest(conn = conn,
                              url = conn@forecast.apiURL,
                              payload = parameters,
                              verbose = verbose)
  AdaptixCheckRequest(request = r, c("200", "201"))
  return(httr::content(r))
}





#' Convert a scenario list returned by AdaptixForecast followed by an AdaptixGetForecastScenarios call to a data frame
#'
#' @param forecast.list a transformed list of scenarios as provided by AdaptixGetForecastScenarios on a forecast response
#' @return a dataframe object of forecasted scenarios
#' @examples
#' AdaptixForecastPointsToDataframe(AdaptixGetForecastScenarios(AdaptixForecast(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", individual = T, number.of.scenarios = 3, verbose = F)))
AdaptixForecastPointsToDataframe <- function(forecast.list) {
  df.xts <- AdaptixForecastPointsToXts(forecast.list)
  df <- as.data.frame(df.xts)
  df$at <- as.POSIXct(rownames(df), tz = "GMT")
  rownames(df) <- NULL
  return(df)
}



#' Convert a scenario list returned by AdaptixForecast followed by an AdaptixGetForecastScenarios call to an Xts object
#'
#' @param forecast.list a transformed list of scenarios as provided by AdaptixGetForecastScenarios on a forecast response
#' @return an XTS object of the forecasted scenarios
#' @examples
#' AdaptixForecastPointsToXts(AdaptixGetForecastScenarios(AdaptixForecast(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", individual = T, number.of.scenarios = 3, verbose = F)))
AdaptixForecastPointsToXts <- function(forecast.list) {
  if(is.null(forecast.list) || length(forecast.list) == 0)
    return(NULL)
  df.xts <- xts(forecast.list[[1]]$points$value, order.by = forecast.list[[1]]$points$at, tzone = "GMT")
  colnames(df.xts) <- as.character(forecast.list[[1]]$probability)
  if(length(forecast.list) > 1) {
    for(i in 2:length(forecast.list)) {
      temp.xts <- xts(forecast.list[[i]]$points$value, order.by = forecast.list[[i]]$points$at, tzone = "GMT")
      colnames(temp.xts) <- as.character(forecast.list[[i]]$probability)
      df.xts <- merge.xts(df.xts, temp.xts)
    }
  }
  return(df.xts)
}






#' Converts scenarios in a forecast response to a comprehensive list structure of scenarios
#'
#' @param forecast.response A forecast response as returned by an AdaptixForecast call.
#' @return transformed list of scenarios
AdaptixGetForecastScenarios <- function(forecast.response) {
  scenarios <- forecast.response$scenarios
  l <- list()
  index <- 1
  if(length(scenarios) == 0)
    return(l)
  for(i in 1:length(scenarios)) {
    #sometimes the API returns empty list of points... so we have to check
    if(length(scenarios[[i]]$points) > 0) {
      df <- data.frame(matrix(unlist(scenarios[[i]]$points), nrow = length(scenarios[[i]]$points), byrow = T), stringsAsFactors = F)
      colnames(df) <- c("value" , "at")
      df$at <- as.POSIXct(df$at, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
      df$value <- as.numeric(df$value)
      occurrences <- scenarios[[i]]$occurrences
      occ <- data.frame()
      for(j in  1:length(occurrences)) {
        occ <- rbind(occ,
                     data.frame(from = as.POSIXct(occurrences[[j]]$from, tz = "GMT") ,
                                to = as.POSIXct(occurrences[[j]]$to, tz = "GMT"),
                                ratio = as.numeric(occurrences[[j]]$ratio)))
      }
      sublist <- list(points = df,
                      occurrences = occ,
                      probability = scenarios[[i]]$probability)

      l[[index]] <- sublist
      index <- index + 1
    }
  }
  return(l)
}
