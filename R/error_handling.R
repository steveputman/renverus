#' Check response for errors
#' @description
#' If the API returns 400, there was a problem with the provided parameters; stop
#' If the API returns 400 and request was to /tokens endpoint, likely bad credentials; stop.
#' If the API returns 401, refresh access token if found and resend request.
#' If the API returns 403 and request was to /tokens endpoint, sleep for 60 seconds and try again.
#' If the API returns 404, an invalid dataset name was provided; stop.
#' 5xx errors are re-sent through a normal `RETRY`
#' @param response HTTP response
#' @return string to determine retry strategy
check_response <- function(response) {
  tokenrequest <- stringr::str_detect(response$url, "tokens")
  if (httr::http_error(response)) {
    if (httr::status_code(response) == 400) {
      if (tokenrequest) {
        httr::stop_for_status(response, "get token")
      } else {
        httr::stop_for_status(response, "complete query. There was a problem with the supplied parameters")
      }
    }
    if (httr::status_code(response) == 401) {
      Sys.setenv("ENVERUS_ACCESS_TOKEN" = "")
      get_access_token()
      httr::message_for_status(response, "authenticate; token missing or expired. Obtaining new token")
      return("manretry")
    }
    if (httr::status_code(response) == 403 & tokenrequest) {
      httr::message_for_status(response, "obtain token. Waiting 60 seconds to retry")
      Sys.sleep(60)
      return("manretry")
    }
    if (httr::status_code(response) == 404) {
      httr::stop_for_status(response, "locate dataset.")
    }
    httr::warn_for_status(response, "connect to server. Retrying")
    return("autoretry")
  } else {
    return("ok")
  }
}
