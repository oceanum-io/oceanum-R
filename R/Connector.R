#' Connector
#'
#' Objected used to establish connection with Datamesh service
#'
#' @param token Character string with the Scopus query
#'
#' @examples
#' Connector(token = yourToken)
#'
#' @import tidyverse
#' @import httr
#' @import rjson
#'
#' @export
#' 

Connector <- R6Class("Connector",
                    public = list(
                      initialize = function(token = Sys.getenv("DATAMESH_TOKEN"), 
                                            service = 'https://datamesh.oceanum.io',
                                            verify = TRUE)){
                      private$
                    }
                    ), 
                    private = list(
                    #token = Sys.getenv("DATAMESH_TOKEN"),
                    #service = 'https://datamesh.oceanum.io',
                    #verify = TRUE) )


