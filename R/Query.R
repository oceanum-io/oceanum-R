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
    filter = "null",
    json = list(),
    initialize = function(list_input = NULL,
                          datasource = NULL,
                          parameters = NULL,
                          description = NULL,
                          variables = NULL,
                          timefilter = NULL,
                          geofilter = NULL,
                          coordfilter = NULL,
                          crs = NULL,
                          aggregate = NULL){

      query_names <- list("datasource",
                         "parameters",
                         "description",
                         "variables",
                         "timefilter",
                         "geofilter",
                         "coordfilter",
                         "crs",
                         "aggregate")

      if(is.list(list_input)){
        self$json = list_input
        var_names <- names(list_input)
        for (i in 1:length(var_names)){
          name = var_names[i]
          self[[name]] <- list_input[[name]]
        }
        if("datasource" %in% var_names){
          datasource = list_input["datasource"]
        }
        if("parameters" %in% var_names){
          parameters = list_input["parameters"]
        }
        if("description" %in% var_names){
          description = list_input["description"]
        }
        if ("variables" %in% var_names) {
          variables <- list_input[["variables"]]
        }
        if ("timefilter" %in% var_names) {
          timefilter <- list_input[["timefilter"]]
        }
        if ("geofilter" %in% var_names) {
          geofilter <- list_input[["geofilter"]]
        }
        if ("coordfilter" %in% var_names) {
          coordfilter <- list_input[["coordfilter"]]
        }
        if ("crs" %in% var_names) {
          crs <- list_input[["crs"]]
        }
        if ("aggregate" %in% var_names) {
          aggregate <- list_input[["aggregate"]]
        }
        self$filter <- paste0('{"datasource":', datasource, ",",
                            '"parameters":', parameters, ",",
                            '"description":', description, ",",
                            '"variables":', variables, ",",
                            '"timefilter":', timefilter, ",",
                            '"geofilter":', geofilter, ",",
                            '"coordfilter":', coordfilter, ",",
                            '"crs":', crs, ",",
                            '"aggregate":', aggregate, "}")
      } else if(is.na(datasource)){
        stop("A datasource must be provided")
      } else {
        self$filter <- paste0('{"datasource":', datasource, ",",
                         '"parameters":', parameters, ",",
                         '"description":', description, ",",
                         '"variables":', variables, ",",
                         '"timefilter":', timefilter, ",",
                         '"geofilter":', geofilter, ",",
                         '"coordfilter":', coordfilter, ",",
                         '"crs":', crs, ",",
                         '"aggregate":', aggregate, "}")
        if(!is.null(datasource)){
          self$json$datasource = datasource
          self$datasource = datasource
        }
        if(!is.null(parameters)){
          self$json$parameters = parameters
        }
        if(!is.null(description)){
          self$json$description = description
        }
        if (!is.null(variables)) {
          self$json$variables <- variables
        }
        if (!is.null(timefilter)) {
          self$json$timefilter <- timefilter
        }
        if (!is.null(geofilter)) {
          self$json$geofilter <- geofilter
        }
        if (!is.null(coordfilter)) {
          self$json$coordfilter <- coordfilter
        }
        if (!is.null(crs)) {
          self$json$crs <- crs
        }
        if (!is.null(aggregate)) {
          self$json$aggregate <- aggregate
        }
      }

    },
    str = function(){
      return(self$filter)
    },
    to_list = function(){
      return(self$json)
    }
  )
)
