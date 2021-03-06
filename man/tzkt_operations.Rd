% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tzkt_api.R
\name{tzkt_operations}
\alias{tzkt_operations}
\alias{tzkt_operations_hash}
\alias{tzkt_bigmap}
\alias{tzkt_quote}
\title{Interact with the TzKT API}
\usage{
tzkt_operations(
  address,
  level = NA,
  limit = NA,
  span = NA,
  quote = "usd",
  base = "https://api.tzkt.io/"
)

tzkt_operations_hash(hash, quote = "usd", base = "https://api.tzkt.io/")

tzkt_bigmap(id, key, base = "https://api.tzkt.io/")

tzkt_quote(level, base = "https://api.tzkt.io/")
}
\arguments{
\item{address}{Tezos address for which to look up operations}

\item{level}{The block level (block number), e.g. 2148000}

\item{limit}{Number of items to return}

\item{span}{Character vector of length 2 defining the time span to look up}

\item{quote}{Currency to use for quotes, e.g. \code{"usd"}}

\item{base}{Base API URL, e.g. \code{"https://api.tzkt.io/"}}

\item{hash}{Operations hash}

\item{id}{Numeric identifier of the bigmap}

\item{key}{Key for key-value lookup in the bigmap.}
}
\description{
Various functions to interact with the TzKT API.
}
\details{
The TzKT Explorer provides a free REST API and WebSocket API for accessing
detailed Tezos blockchain data and helps developers build more services
and applications on top of Tezos. The functions included here were built for
TzKT API (1.7.0).

Additional API documentation is available at: https://api.tzkt.io/
}
\examples{
# fetch all operations for a given address
address <- "tz1cnfEeha6cibgKVs6DTGKyrP9V4bz5F2N6"
tzkt_operations(address)
# Fetch data for a given transaction
tzkt_operations_hash("onujxsRLwcYZuKVTKG1JLwFEe9EWBSPdiB6pRCg5WcH6NpXdRxA")
# Look up info for fxhash project #5577, in_vitro by Xeronimo
# https://www.fxhash.xyz/generative/slug/in_vitro
tzkt_bigmap(70072, 5577)
# Look up a quote for block number 2148000
tzkt_quote(level = 2148000)
}
