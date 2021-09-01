apiurl <- "https://di-api.drillinginfo.com/v2/direct-access"

#' @export
enverus2 <- function(dataset, ...) {
  access_token <- Sys.getenv("ENVERUS_ACCESS_TOKEN")
  api_key <- Sys.getenv("ENVERUS_API_KEY")
  options = list(...)
  apiurl <- "https://di-api.drillinginfo.com/v2/direct-access"
  queryurl <- glue::glue("{apiurl}/{dataset}")
  queryresult <- httr::GET(queryurl,
                             encode = "json",
                             httr::add_headers("X-API-KEY" = api_key,
                                               "Authorization" = glue::glue("Bearer {access_token}")),
                             httr::verbose())
  check_response(queryresult)

}
