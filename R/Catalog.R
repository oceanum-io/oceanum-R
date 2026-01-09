#' Catalog
#'
#' Object used to hold and organise Catalog information.
#'
#' @export
Catalog <- R6::R6Class(
  "Catalog",

  public = list(
    geojson    = NULL,
    ids        = NULL,
    Extent     = NULL,
    properties = NULL,
    connector  = NULL,

    initialize = function(geojsondata, connector) {
      self$geojson   <- geojsondata
      self$connector <- connector
      if (is.null(geojsondata)){
        base::return()
      }

      if (!is.null(geojsondata$features$id) &&
          length(geojsondata$features$id) > 0) {

        self$ids        <- geojsondata$features$id
        self$Extent     <- geojsondata$features$bbox
        self$properties <- geojsondata$features$properties
      }
    },
    get_datasource = function(datasource_id){
      self$connector$get_datasource(datasource_id)
    },

    query = function(query_input){
      self$connector$query(query_input)
    },

    print = function(...) {
      search_size = length(self$ids)
      base::print(paste0("Datamesh catalog with ", search_size,  " datasources:"))
      if (!is.null(self$ids) && search_size > 0) {
        for (i in seq_along(self$ids)) {
          if (!is.null(self$properties$name[i])){
            name = self$properties$name[i]
          } else {
            name = self$ids[i]
          }
          # Format catalog information so its understandable
          cat(
            format(name), " ",
            "[",format(self$ids[i]), "]", "\n",
            "Extent: ",
            format(self$Extent[i]), "\n",
            "Timerange: ",
            format(self$properties$tstart[i]), " to ",
            format(self$properties$tend[i]), "\n\n",
            sep = ""
          )
        }
      }
      invisible(self)
    }
  )
)
