#' Global ACME options
#'
#' Get and set global ACME options.
#'
#' @export
#' @param verbose emit verbose http output
#' @param server acme server endpoint root
acme_options <- local({
  VERBOSE = TRUE
  SERVER = 'https://acme-v01.api.letsencrypt.org/'
  function(verbose = NULL, server = NULL){
    if(length(verbose))
      VERBOSE <<- verbose
    if(length(server))
      SERVER <<- server
    list(
      verbose = VERBOSE,
      server = SERVER
    )
  }
})

GET <- function(path){
  fetch(path, handle = handle())
}

HEAD <- function(path){
  fetch(path, handle = handle(nobody = TRUE))
}

POST <- function(path, data){
  fetch(path, handle = handle(postfields = TRUE))
}

fetch <- function(path, handle){
  url <- acme_url(path)
  curl::handle_setopt(handle, verbose = acme_options()$verbose)
  req <- curl::curl_fetch_memory(url, handle)
  if(req$status >= 400)
    stop(sprintf("HTTP %d: %s", req$status, rawToChar(req$content)), call. = FALSE)
  jsonlite::fromJSON(rawToChar(req$content))
}

handle <- function(...){
  handle <- curl::new_handle(
    useragent = paste(R.version.string, "/ curl", utils::packageVersion('curl'), "/ acme", utils::packageVersion('acme')))
}

acme_url <- function(endpoint, server = acme_options()$server) {
  url_path(c(server, endpoint))
}

url_path <- function(pieces){
  do.call(file.path, as.list(c(pieces, fsep = "/")))
}
