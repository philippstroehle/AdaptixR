% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stream.R
\name{AdaptixGetStreamsExpanded}
\alias{AdaptixGetStreamsExpanded}
\title{List all Adaptix streams, with expanded information.}
\usage{
AdaptixGetStreamsExpanded(conn, verbose = FALSE)
}
\arguments{
\item{conn}{a valid Adaptix connection object.}

\item{verbose}{display HTTP operation details.}
}
\value{
a listing of streams, in list format.
}
\description{
List all Adaptix streams, with expanded information.
}
\examples{
AdaptixGetStreamsExpanded(conn = conn, verbose = F)
}
