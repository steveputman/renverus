#' query
#'
#' @description
#' Query for the v2 API
#'
#' @param dataset A valid Enverus DirectAccess v2 API dataset as a string. For a
#' complete list of datasets, see [Enverus API](https://app.drillinginfo.com/direct/#/api/explorer/v2/gettingStarted)
#'
#' @param api_key A valid Enverus DirectAccess v2 API key as a string. If a key
#' is not provided, the query will attempt to use the `ENVERUS_API_KEY`
#' environment variable.
#'
#' @param access_token A temporary token, as a string, authenticating the user.
#' If a token is not supplied, the query will attempt to authenticate the user
#' with the `ENVERUS_CLIENT_ID` and `ENVERUS_CLIENT_SECRET` environment
#' variables. See [get_access_token()]
#'
#' @param output A string indicating the type of output returned by the query:
#'
#'  - `df` attempts to convert the query response into a `tibble` (the default).
#'  - `json` returns a raw list of lists
#'
#' @param manretry Number of retries for user-related errors (authentication,
#' query parameters, etc.)
#'
#' @param ... A named list of query filters, such as `deleteddate = "null"` to
#' be passed to the query. See the [Enverus API](https://app.drillinginfo.com/direct/#/api/explorer/v2/gettingStarted)
#'
#' @export

query <- function(dataset, output = "df", manretry = 2, api_key = NULL,
                  access_token = NULL, ...) {
  if (is.null(access_token)) access_token <- Sys.getenv("ENVERUS_ACCESS_TOKEN")
  if (is.null(api_key)) api_key <- Sys.getenv("ENVERUS_API_KEY")
  update_httr_config(api_key, access_token, TRUE)
  filters <- list(...)
  queryurl <- glue::glue("{baseurl}/{dataset}")
  queryresponse <- httr::GET(queryurl,
    query = filters,
    encode = "json"
  )

  errors <- check_response(queryresponse)
  if (errors == "manretry") {
    if (manretry > 0) {
      manretry <- manretry - 1
      query(dataset, manretry, ...)
    } else {
      stop("Still failing; stopping.")
    }
  }
  if (errors == "autoretry") {
    queryresponse <- httr::RETRY("GET",
      queryurl,
      encode = "json"
    )
  }
  if (errors == "ok") {
    content <- httr::content(queryresponse)
    while (has_link(queryresponse) == TRUE) {
      print(queryresponse$headers$links)
      nexturl <- stringr::str_match(queryresponse$headers$links, "<(.*?)>")[[2]]
      nextfull <- paste0(baseurl, nexturl)
      # Deals with "next" link headers that match last-retrieved url
      if (queryurl == nextfull) break
      queryurl <- nextfull
      queryresponse <- httr::GET(queryurl,
        encode = "json"
      )
      content <- c(content, httr::content(queryresponse))
    }
  }
  if (output == "df") {
    parsed <- makedf(content)
  } else {
    parsed <- content
  }
  return(parsed)
}

makedf <- function(content) {
  df <- purrr::map_dfr(content, purrr::flatten_dfr)
  return(df)
}

makecsv <- function(content) {

}
