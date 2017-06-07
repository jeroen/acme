# A nonce should be included with each POST to protect against replay attacks
NONCE <- NULL

set_nonce <- function(nonce){
  NONCE <<- nonce
}

get_nonce <- function(){
  if(!length(NONCE))
    acme_new_nonce()
  if(!length(NONCE))
    bail("Failed to get new nonce")
  nonce <- NONCE
  set_nonce(NULL)
  return(nonce)
}

acme_new_nonce <- function(){
  HEAD("/acme/new-nonce")
}

bail <- function(...){
  stop(sprintf(...), call. = FALSE)
}
