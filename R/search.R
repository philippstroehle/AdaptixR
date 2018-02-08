

#' Perform a pattern search.
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream from which to retrieve the points.
#' @param from if not NULL, the starting date of the pattern.
#' @param to if not NULL, the closing date of the pattern.
#' @param similarity.ratio the similarity ratio of the pattern search [0.0 to 1.0].
#' @param historical.depth.from currently ignored
#' @param historical.depth.to currently ignored
#' @param in.streams if not NULL, the pattern extracted form the specified "stream" will also be searched in other streams, identified by their ids (provide as a vector of ids)
#' @param verbose display HTTP operation details.
#' @return a data.frame with the requested collection of points.
#' @examples
#' AdaptixSearchPattern(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", similarity.ratio = 0.95, verbose = F)
AdaptixSearchPattern <- function(conn, stream, from, to, similarity.ratio = 1.0, historical.depth.from = NULL, historical.depth.to = NULL, in.streams = NULL, verbose = FALSE)
{
  from <- ConvertDateToISO8601(from)
  to <- ConvertDateToISO8601(to)

  json_payload <- list(stream_id = stream ,
                       from = from,
                       to = to,
                       ratio = similarity.ratio)
  if(!is.null(in.streams))
    json_payload[["in_streams"]] <- in.streams

  r <- AdaptixPostHTTPRequest(conn = conn, url = conn@search.apiURL, payload = json_payload, verbose = verbose)
  AdaptixCheckRequest(request = r, "200")
  return(httr::content(r))
}
