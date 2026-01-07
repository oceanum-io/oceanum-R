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
  public = list(
        coordinates = NULL,
        created= NULL,
        dataschema= NULL,
        description= NULL,
        details= NULL,
        driver= NULL,
        driver_args= NULL,
        expires= NULL,
        geom= NULL,
        id= NULL,
        info= NULL,
        labels= NULL,
        modified= NULL,
        name= NULL,
        parameters= NULL,
        parchive= NULL,
        pforecast= NULL,
        tags= NULL,
        tend= NULL,
        tstart= NULL,
        # ------ Properties ------
        attributes= NULL,
        bounds= NULL,
        geometry= NULL,
        variables= NULL,
        initialize = function(props){
          prop_names = names(props)
          if("coordinates" %in% prop_names){
            self$coordinates = props$coordinates
          }

          if("created" %in% prop_names){
            self$created = props$created
          }

          if("dataschema" %in% prop_names){
            self$dataschema = props$dataschema
          }else if("schema" %in% prop_names){
            self$dataschema = props$schema
          }

          if("description" %in% prop_names){
            self$description = props$description
          }

          if("details" %in% prop_names){
            self$details = props$details
          }

          if("driver" %in% prop_names){
            self$driver = props$driver
          }

          if("driver_args" %in% prop_names){
            self$driver_args = props$driver_args
          }else if("args" %in% prop_names){
            self$driver_args = props$args
          }

          if("expires" %in% prop_names){
            self$expires = props$expires
          }

          if("geom" %in% prop_names){
            self$geom = props$geom
          }
          if("id" %in% prop_names){
            self$id = props$id
          }
          if("info" %in% prop_names){
            self$info = props$info
          }
          if("labels" %in% prop_names){
            self$labels = props$labels
          }
          if("modified" %in% prop_names){
            self$modified = props$modified
          }
          if("name" %in% prop_names){
            self$name = props$name
          }
          if("parameters" %in% prop_names){
            self$parameters = props$parameters
          }

          if("parchive" %in% prop_names){
            self$parchive = props$parchive
          }

          if("pforecast" %in% prop_names){
            self$pforecast = props$pforecast
          }
          if("tags" %in% prop_names){
            self$tags = props$tags
          }
          if("tend" %in% prop_names){
            self$tend = props$tend
          }
          if("tstart" %in% prop_names){
            self$tstart = props$tstart
          }
          if("metadata" %in% prop_names){
            self$attributes = props$metadata
          }
          # TODO: Implement GeoJSON display of bounds??

          if("bbox" %in% prop_names){
            self$bounds = props$bbox
          }
          if("schema" %in% prop_names){
            self$variables = props$schema$data_vars
          }
        }
  )
)
