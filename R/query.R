#' @export
query <- function(dataset, manretry = 2, ...) {
  access_token <- Sys.getenv("ENVERUS_ACCESS_TOKEN")
  api_key <- Sys.getenv("ENVERUS_API_KEY")
  update_httr_config(api_key, access_token, TRUE)
  filters = list(...)
  queryurl <- glue::glue("{baseurl}/{dataset}")
  queryresponse <- httr::GET(queryurl,
                             query = filters,
                             encode = "json")

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
                                 encode = "json")
  }
  if (errors == "ok") {
    content <- httr::content(queryresponse)
    x <- 1
    while ((has_link(queryresponse) == TRUE) & x < 10) {
      nexturl <- stringr::str_match(queryresponse$headers$links, "<(.*?)>")[[2]]
      queryurl <- glue::glue("{baseurl}{nexturl}")
      queryresponse <- httr::GET(queryurl,
                                 encode = "json")
      content <- c(content, httr::content(queryresponse))
      x <- x + 1
    }
    return(content)
  }
}


#' @export
enverus2 <- function(dataset, ...) {
  initresp <- query(dataset, ...)
}

testget <- function(statuscode) {
  resp <- httr::GET(glue::glue("https://httpbin.org/status/{statuscode}"))
  check_response(resp)
  message("am i still going?")
}

maketibble <- function(content) {
  df <- purrr::map_dfr(content, purrr::flatten_dfr)
}
