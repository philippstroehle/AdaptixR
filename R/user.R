
#' Fetch the information of the current user.
#'
#' @param conn a valid Adaptix connection object.
#' @param verbose display HTTP operation details.
#' @return the user information, as a list
#' @examples
#' AdaptixGetUserWhoAmI(conn = conn, verbose = T)
AdaptixGetUserWhoAmI <- function(conn, verbose = FALSE) {
  r <- AdaptixGetHTTPRequest(conn = conn, url = paste0(conn@users.apiURL, "whoami"), verbose = verbose)
  AdaptixCheckRequest(request = r, "200")
  return(httr::content(r))
}


