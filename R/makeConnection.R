#' makeConnection
#'
#' Function used to establish connection with datamesh service
#'
#' @param token Character string with the Scopus query
#'
#' @examples
#' connection(token = yourToken)
#'
#' @import tidyverse
#' @import httr
#' @import rjson
#'
#' @export

makeConnection <- function(token = NULL){

  if(is.null(token)){
    stop("A valid key must be supplied as a connection constructor argument or defined in environment variables as DATAMESH_TOKEN")
  }


}
