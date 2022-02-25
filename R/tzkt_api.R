#' Interact with the TzKT API
#'
#' Various functions to interact with the TzKT API.
#'
#' The TzKT Explorer provides a free REST API and WebSocket API for accessing
#' detailed Tezos blockchain data and helps developers build more services
#' and applications on top of Tezos. The functions included here were built for
#' TzKT API (1.7.0).
#'
#' Additional API documentation is available at: https://api.tzkt.io/
#'
#' @param address Tezos address for which to look up operations
#' @param level The block level (block number), e.g. 2148000
#' @param limit Number of items to return
#' @param span Character vector of length 2 defining the time span to look up
#' @param quote Currency to use for quotes, e.g. `"usd"`
#' @param base Base API URL, e.g. `"https://api.tzkt.io/"`
#' @examples
#' # fetch all operations for a given address
#' address <- "tz1cnfEeha6cibgKVs6DTGKyrP9V4bz5F2N6"
#' tzkt_operations(address)
#' @export
tzkt_operations <- function(address, level = NA, limit = NA,
                            span = NA, quote = "usd",
                            base = "https://api.tzkt.io/") {
  # Get account operations, https://api.tzkt.io/#operation/Accounts_GetOperations
  sfx <- paste0("v1/accounts/", address, "/operations?quote=", quote)
  if (!is.na(level)) sfx <- paste0(sfx, "&level.lt=", level)
  if (!is.na(limit)) sfx <- paste0(sfx, "&limit=", limit)
  if (!is.na(span)[1]) {
    sfx <- paste0(sfx, "&timestamp.ge=", span[1], "&timestamp.le=", span[2])
  }
  url <- paste0(base, sfx)
  jsonlite::fromJSON(url)
}

#' @rdname tzkt_operations
#' @param hash Operations hash
#' @examples
#' # Fetch data for a given transaction
#' tzkt_operations_hash("onujxsRLwcYZuKVTKG1JLwFEe9EWBSPdiB6pRCg5WcH6NpXdRxA")
#' @export
tzkt_operations_hash <- function(hash, quote = "usd", base = "https://api.tzkt.io/") {
  # Get operations by hash, https://api.tzkt.io/#operation/Operations_GetByHash
  sfx <- paste0("v1/operations/", hash, "?quote=", quote)
  url <- paste0(base, sfx)
  jsonlite::fromJSON(url)
}

#' @rdname tzkt_operations
#' @param id Numeric identifier of the bigmap
#' @param key Key for key-value lookup in the bigmap.
#' @examples
#' # Look up info for fxhash project #5577, in_vitro by Xeronimo
#' # https://www.fxhash.xyz/generative/slug/in_vitro
#' tzkt_bigmap(70072, 5577)
#' @export
tzkt_bigmap <- function(id, key, base = "https://api.tzkt.io/") {
  # Get bigmap by ID, https://api.tzkt.io/#operation/BigMaps_GetBigMapById
  sfx <- paste0("v1/bigmaps/", id, "/keys/", key)
  url <- paste0(base, sfx)
  jsonlite::fromJSON(url)
}

#' @rdname tzkt_operations
#' @examples
#' # Look up a quote for block number 2148000
#' tzkt_quote(level = 2148000)
#' @export
tzkt_quote <- function(level, base = "https://api.tzkt.io/") {
  # Get quotes, https://api.tzkt.io/#operation/Quotes_Get
  sfx <- paste0("v1/quotes?level=", level)
  url <- paste0(base, sfx)
  jsonlite::fromJSON(url)
}


