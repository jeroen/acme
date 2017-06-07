#' ACME wrappers
#'
#' Wrappers for ACME server endpoints.
#'
#' @rdname acme
#' @name acme
#' @export
acme_directory <- function(){
  GET("/directory")
}
