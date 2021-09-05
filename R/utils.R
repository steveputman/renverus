validate_auth_info <- function(api_key, client_id, client_secret) {
  if (anyNA(c(api_key, client_id, client_secret))) {
    stop("`api_key`, `client_id`, and`client_secret` must be supplied as arguments or set as environment variables.")
  }
  if (!is.character(api_key) | !is.character(client_id) | !is.character(client_secret)) {
    stop("`api_key`, `client_id`, and `client_secret` must be strings.")
  }
}

get_auth_vars <- function(api_key = NULL, client_id = NULL, client_secret = NULL) {
  if (is.null(api_key)) api_key <- Sys.getenv("ENVERUS_API_KEY")
  if (is.null(client_id)) client_id <- Sys.getenv("ENVERUS_CLIENT_ID")
  if (is.null(client_secret)) client_secret <- Sys.getenv("ENVERUS_CLIENT_SECRET")
  validate_auth_info(api_key, client_id, client_secret)
  return(list(api_key = api_key, client_id = client_id, client_secret = client_secret))
}

has_link <- function(response) {
  if (length(response$headers$links) > 0) {
    if (stringr::str_detect(response$header$links, "next")) return(TRUE)
  }
  return(FALSE)
}


update_httr_config <- function(api_key, access_token, verbose = FALSE) {
  httr::set_config(
    c(httr::add_headers("X-API-KEY" = api_key,
                      "Authorization" = glue::glue("Bearer {access_token}")),
    httr::user_agent(useragent))
  )
  if (verbose == TRUE) {
    httr::set_config(
      httr::verbose()
    )
  }
}


