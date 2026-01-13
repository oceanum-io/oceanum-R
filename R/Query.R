#' Query
#'
#' Objected used to hold and organise query information.
#'
#' @param data
#'
#' @examples
#' Query$new(data)
#'
#' @import R6
#' @import tidyverse
#' @import httr2
#' @import rjson
#'
#' @export

Query <- R6::R6Class(
  "Query",
  private = list(

  ),
  public = list(
    datasource = list(
      title = "The id of the datasource",
      description = "Datasource ID",
      min_length = 3,
      max_length = 80
    ),
    parameters = list(
      title = "Datasource parameters",
      default = list(),
      description = "Dictionary of driver parameters to pass to datasource"
    ),
    description = list(
      title = "Optional description of this query",
      default = NULL,
      description = "Human readable description of this query"
    ),
    variables = list(
      title = "List of selected variables",
      default = NULL,
      description = "List of requested variables."
    ),
    timefilter = list(
      title = "Time filter",
      default = NULL,
      description = "Temporal filter or interplator"
    ),
    geofilter = list(
      title = "Spatial filter or interpolator",
      default = NULL
    ),
    coordfilter = list(
      title = "List of additional coordinate filters",
      default = NULL
    ),
    crs = list(
      title = "Spatial reference for filter and output",
      default = NULL,
      description = "Valid CRS string for returned data"
    ),
    aggregate = list(
      title = "Aggregation operators to apply",
      default = NULL,
      description = "Optional aggregation operators to apply to query after filtering"
    ),
    initialize = function(list_input = NULL,
                          datasource = NULL,
                          parameters = "{}",
                          description = "null",
                          variables = "null",
                          timefilter = "null",
                          geofilter = "null",
                          coordfilter = "null",
                          crs = "null",
                          aggregate = "null"){
      if(is.list(list_input)){
        var_names <- names(list_input)


      }

    }

  )
)
