#' Global ACME options
#'
#' Get and set global ACME options.
#'
#' @export
#' @param verbose emit verbose http output
#' @param server acme server endpoint root
acme_options <- local({
  VERBOSE = TRUE
  SERVER = 'https://acme-v01.api.letsencrypt.org'
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
  fetch(path, handle = acme_handle())
}

HEAD <- function(path){
  fetch(path, acme_handle(nobody = TRUE))
}

POST <- function(path, data){
  handle <- acme_handle(postfields = jsonlite::toJSON(data))
  # TODO: set a nonce
  fetch(path, handle = handle)
}

find_header <- function(name, headers){
  pattern <- paste0("^", name, "\\s*:\\s*")
  values <- grep(pattern, headers, ignore.case = TRUE, value = TRUE)
  sub(pattern, "", values, ignore.case = TRUE)
}

fetch <- function(path, handle){
  url <- acme_url(path)
  handle <- curl::handle_setopt(handle, verbose = acme_options()$verbose)
  req <- curl::curl_fetch_memory(url, handle)
  if(req$status >= 400)
    bail("HTTP %d (%s): %s", req$status, path, rawToChar(req$content))
  nonce <- find_header("replay-nonce", parse_headers(res$headers))
  if(length(nonce))
    set_nonce(nonce)
  if(length(req$content))
    jsonlite::fromJSON(rawToChar(req$content))
}

acme_handle <- function(...){
  curl::new_handle(...,
    useragent = paste(R.version.string, "/ curl", utils::packageVersion('curl'), "/ acme", utils::packageVersion('acme')))
}

acme_url <- function(endpoint, server = acme_options()$server) {
  paste0(server, url_path(endpoint))
}

url_path <- function(pieces){
  url <- do.call(file.path, as.list(c(pieces, fsep = "/")))
  gsub("/+", "/", url)
}
