baseurl <- "https://api.enverus.com/v3/direct-access"
useragent <- "https://github.com/steveputman/renverus"

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
  req <- httr2::request(baseurl) |>
    httr2::req_url_path("tokens") |>
    httr2::req_body_json(list(secretKey = api_key))




  response <- httr::POST(glue::glue("{baseurl}/tokens"),
    body = list("grant_type" = "client_credentials"),
    encode = "form",
    httr::user_agent(useragent),
    httr::add_headers("secretKey" = api_key),
  )
  resp_status <- check_response(response)
  if (resp_status == "ok") {
    token <- httr::content(response)$access_token
    Sys.setenv("ENVERUS_ACCESS_TOKEN" = token)
    invisible(token)
  }
}
