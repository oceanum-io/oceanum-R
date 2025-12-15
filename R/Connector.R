#' Connector
#'
#' Objected used to establish connection with Datamesh service
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
#' 

Connector <- R6Class(token = Sys.getenv("DATAMESH_TOKEN"), 
                      service = 'https://datamesh.oceanum.io', 
                      verify = TRUE) {
  
  
  
}


