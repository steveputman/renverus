#' Authentication
#'
#' @description
#' Users of Enverus DirectAccess v2 must authenticate with an API key and a token
#' generated from their client ID and client secret. The API key, client ID, and
#' client secret may be stored as environment variables, respectively, or passed as
#' arguments.
#'
#' - `get_access token()` will retrieve a token given the `api_key`, `client_id`,
#'
#' @param api_key A valid Enverus DirectAccess v2 API key as a string. Obtain one at the [API
#'
#' @param client_id A valid Enverus DirectAccess v2 client ID as a string.
#'
#' @param client_secret A valid Enverus DirectAccess v2 client secret as a string
#'
#' @details
#'   The preferred method to set the key is to set the `ENVERUS_API_KEY`,
#'   `ENVERUS_CLIENT_ID`, and `ENVERUS_CLIENT_SECRET` environment variables in an
#'   `.Renviron` file. The easiest way to do this is by calling
#'   `usethis::edit_r_environ()`. Don't forget to restart R after saving the variables.
#'
#' @references See [enverus link] to obtain an API key.
#'
#' @seealso Note that by using a Enverus API key, you agree to the Enverus API [Terms
#'   of Use]([   ]).
#'
#' @name
#'
#' @examples

#' @export
#'
get_access_token <- function(api_key = Sys.getenv("ENVERUS_API_KEY"), client_id =
                               Sys.getenv("ENVERUS_CLIENT_ID"), client_secret =
                               Sys.getenv("ENVERUS_CLIENT_SECRET")) {
  if (anyNA(c(api_key, client_id, client_secret))) {
    abort("`api_key`, `client_id`, and:`client_secret` must be strings.")
  }
  token_url <- "https://di-api.drillinginfo.com/v2/direct-access/tokens"
  response <- httr::POST(token_url,
                        body = list("grant_type" = "client_credentials"),
                        encode = "form",
                        httr::add_headers("X-API-KEY" = api_key),
                        httr::authenticate(client_id, client_secret),
                        httr::verbose())
  resp_status <- check_response(token)
  if (resp_status == "ok") {
    token <- httr::content(request)$access_token
    Sys.setenv("ENVERUS_ACCESS_TOKEN" = token)
  }
}

