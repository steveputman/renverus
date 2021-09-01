#' Check response for errors
#' @description
#' If the API returns 400, there was a problem with the provided parameters. Raise DAQueryException.
#' If the API returns 400 and request was to /tokens endpoint, likely bad credentials. Raise DAAuthException.
#' If the API returns 401, refresh access token if found and resend request.
#' If the API returns 403 and request was to /tokens endpoint, sleep for 60 seconds and try again.
#' If the API returns 404, an invalid dataset name was provided. Raise DADatasetException.
#' 5xx errors are handled by the session's Retry configuration. Debug logging returns retries remaining.
#' @param response [xx-formatted] HTTP response
#' @return Any error messages
check_response <- function(response, querydataset, ...) {
  tokenrequest <- stringr::str_detect(response$url, "tokens")
  if (httr::http_error(response)) {
    if (httr::status_code(response) == 400) {
      if (tokenrequest) {
        httr::stop_for_status(response, "get token")
      } else {
        httr::stop_for_status(response, "complete query")
      }
    }
    if (httr::status_code(response) == 401) {
      Sys.setenv("ENVERUS_ACCESS_TOKEN" = "")
      get_access_token()
      httr::message_for_status(response, "authenticate; token missing or expired. Obtaining new token")
      query(querydataset, ...)
    }
    if (httr::status_code(response) == 403 & tokenrequest) {
      httr::message_for_status(response, "obtain token. Waiting 60 seconds to retry")
      Sys.sleep(60)
      query(querydataset, ...)
    }
    if (httr::status_code(response) == 404) {
      httr::warn_for_status(response, "locate dataset.")
    }
  } else {
    return(response)
  }
}
