#' Stage
#'
#' Objected used to hold and organise staging information.
#'
#' @param data
#'
#' @examples
#' Stage$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import httr2
#' @import rjson
#'
#' @export

Stage <- R6::R6Class(
  "Stage",
  public = list(
    query = NULL,
    qhash = NULL,
    formats = NULL,
    size = NULL,
    dlen = NULL,
    coordmap = NULL,
    coordkeys = NULL,
    container = NULL,
    sig = NULL,
    initialize = function(data){
      data_names = names(data)
      if("query" %in% data_names){
        self$query = data$query
      }
      if("qhash" %in% data_names){
        self$qhash = data$qhash
      }
      if("formats" %in% data_names){
        self$formats = data$formats
      }
      if("size" %in% data_names){
        self$size = data$size
      }
      if("dlen" %in% data_names){
        self$dlen = data$dlen
      }
      if("coordmap" %in% data_names){
        self$coordmap = data$coordmap
      }
      if("container" %in% data_names){
        self$container = data$container
      }
      if("sig" %in% data_names){
        self$sig = data$sig
      }
    }
  )
)
