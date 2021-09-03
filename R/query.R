#' @export
query <- function(dataset, manretry = 2, ...) {
  access_token <- Sys.getenv("ENVERUS_ACCESS_TOKEN")
  api_key <- Sys.getenv("ENVERUS_API_KEY")
  filters = list(...)
  apiurl <- "https://di-api.drillinginfo.com/v2/direct-access"
  queryurl <- glue::glue("{apiurl}/{dataset}")
  queryresponse <- httr::GET(queryurl,
                             query = filters,
                             encode = "json",
                             httr::add_headers("X-API-KEY" = api_key,
                                               "Authorization" = glue::glue("Bearer {access_token}")),
                             httr::verbose())
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
                                 encode = "json",
                                 httr::add_headers("X-API-KEY" = api_key,
                                                   "Authorization" = glue::glue("Bearer {access_token")),
                                 httr::verbose())
  }
  if (errors == "ok") {
    content <- httr::content(queryresponse)
    while (stringr::str_detect(queryresponse$headers$links, "next")) {
      nexturl <- stringr::str_match(queryresponse$headers$links, "<(.*?)>")[[2]]
      queryurl <- glue::glue(apiurl, nexturl)
      queryresponse <-   httr::GET(queryurl,
                                   encode = "json",
                                   httr::add_headers("X-API-KEY" = api_key,
                                   "Authorization" = glue::glue("Bearer {access_token}")),
                                    httr::verbose())
      content <- c(content, httr::content(queryresponse))
    }
    return(content)
  }
}


#' @export
enverus2 <- function(dataset, ...) {
  initresp <- query(dataset, ...)
}

#' @export
testget <- function(statuscode) {
  resp <- httr::GET(glue::glue("https://httpbin.org/status/{statuscode}"))
  check_response(resp)
  message("am i still going?")
}

#' @export
maketibble <- function(content) {
  df <- purrr::map_dfr(content, purrr::flatten_dfr)
}
