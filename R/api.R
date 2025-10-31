basereq <- httr2::request("https://api.enverus.com/v3/direct-access") |>
  httr2::req_user_agent("https://github.com/steveputman/renverus")

#' get_access_token
#'
#' @description
#' Users of Enverus DirectAccess v3 must authenticate with an API key and a
#' token generated from their client ID and client secret. The API key, client
#' ID, and client secret may be stored as environment variables, respectively,
#' or passed as
#' arguments.
#'
#' - `get_access token()` will retrieve a token given the `api_key`,
#' `client_id`, and `client_secret`
#'
#' @param api_key A valid Enverus DirectAccess v3 API key as a string. Obtain
#' one at the
#' [API site](https://app.drillinginfo.com/direct/#/api/explorer/v3/gettingStarted)
#'
#' @details
#' The preferred method to set the key is to set the `ENVERUS_API_KEY`  environment
#' variables in an `.Renviron` file. The easiest way to do this is by calling
#' `usethis::edit_r_environ()`. Don't forget to restart R after saving the
#' variables.
#'
#' @return Returns he retrieved token invisibly and sets the
#' `ENVERUS_ACCESS_TOKEN` environment variable.
#'
#' @export
#'
get_access_token <- function(api_key = NULL) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ENVERUS_V3_API_KEY")
  }
  req <- basereq |>
    httr2::req_url_path_append("tokens") |>
    httr2::req_body_json(list(secretKey = api_key))

  resp <- httr2::req_perform(req)
  if (httr2::resp_is_error(resp)) {
    stop("Error: ", httr2::resp_status_desc(resp), call. = FALSE)
  } else {
    token <- httr2::resp_body_json(resp)$token
    Sys.setenv("ENVERUS_V3_ACCESS_TOKEN" = token)
    invisible(token)
  }
}

test_call <- function(token = NULL) {
  if (is.null(token)) {
    token <- Sys.getenv("ENVERUS_V3_ACCESS_TOKEN")
  }
  req <- basereq |>
    httr2::req_url_path_append("rigs") |>
    httr2::req_url_query(deleteddate = "null") |>
    httr2::req_auth_bearer_token(token)

  resp <- httr2::req_perform(req)
  if (httr2::resp_is_error(resp)) {
    stop("Error: ", httr2::resp_status_desc(resp), call. = FALSE)
  } else {
    return(TRUE)
  }
}



