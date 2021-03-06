% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user.R
\name{AdaptixGetUserWhoAmI}
\alias{AdaptixGetUserWhoAmI}
\title{Fetch the information of the current user.}
\usage{
AdaptixGetUserWhoAmI(conn, verbose = FALSE)
}
\arguments{
\item{conn}{a valid Adaptix connection object.}

\item{verbose}{display HTTP operation details.}
}
\value{
the user information, as a list
}
\description{
Fetch the information of the current user.
}
\examples{
AdaptixGetUserWhoAmI(conn = conn, verbose = T)
}
