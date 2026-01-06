#' Datasource
#'
#' Objected used to hold and organise datasource information.
#'
#' @param data 
#'
#' @examples
#' Datasource$new(data)
#' 
#' @import R6
#' @import tidyverse
#' @import httr2
#' @import rjson
#'
#' @export

Datasource <- R6::R6Class(
  "Datasource",
)
