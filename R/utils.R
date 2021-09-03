#' @export
has_link <- function(response) {
  if (length(response$headers$links) > 0) {
    if (stringr::str_detect(response$header$links, "next")) return(TRUE)
  }
  return(FALSE)
}
