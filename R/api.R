baseurl <- "https://di-api.drillinginfo.com/v2/direct-access"
useragent <- "https://github.com/steveputman/renverus"

#' get_access_token
#'
#' @description
#' Users of Enverus DirectAccess v2 must authenticate with an API key and a
#' token generated from their client ID and client secret. The API key, client
#' ID, and client secret may be stored as environment variables, respectively,
#' or passed as
#' arguments.
#'
#' - `get_access token()` will retrieve a token given the `api_key`,
#' `client_id`, and `client_secret`
#'
#' @param api_key A valid Enverus DirectAccess v2 API key as a string. Obtain
#' one at the
#' [API site](https://app.drillinginfo.com/direct/#/api/explorer/v2/gettingStarted)
#'
#' @param client_id A valid Enverus DirectAccess v2 client ID as a string.
#'
#' @param client_secret A valid Enverus DirectAccess v2 client secret as a
#' string
#'
#' @details
#' The preferred method to set the key is to set the `ENVERUS_API_KEY`,
#' `ENVERUS_CLIENT_ID`, and `ENVERUS_CLIENT_SECRET` environment variables in an
#' `.Renviron` file. The easiest way to do this is by calling
#' `usethis::edit_r_environ()`. Don't forget to restart R after saving the
#' variables.
#'
#' @return Returns he retrieved token invisibly and sets the
#' `ENVERUS_ACCESS_TOKEN` environment variable.
#'
#' @export
#'
get_access_token <- function(api_key = NULL,
                             client_id = NULL,
                             client_secret = NULL) {
  authvars <- get_auth_vars(api_key, client_id, client_secret)
  api_key <- authvars$api_key
  client_id <- authvars$client_id
  client_secret <- authvars$client_secret
  httr::reset_config()
  response <- httr::POST(glue::glue("{baseurl}/tokens"),
    body = list("grant_type" = "client_credentials"),
    encode = "form",
    httr::user_agent(useragent),
    httr::add_headers("X-API-KEY" = api_key),
    httr::authenticate(client_id, client_secret)
  )
  resp_status <- check_response(response)
  if (resp_status == "ok") {
    token <- httr::content(response)$access_token
    Sys.setenv("ENVERUS_ACCESS_TOKEN" = token)
    invisible(token)
  }
}
