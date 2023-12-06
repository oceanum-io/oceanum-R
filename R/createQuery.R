#' createQuery
#'
#' Function used to establish connection with datamesh service
#'
#' @param theDatasource The id of the datasource
#' @param parameters Datasource parameters
#' @param description Optional description of this query
#' @param variables List of selected variables
#' @param timefilter Time filter
#' @param geofilter Spatial filter or interpolator
#' @param coordfilter List of additional coordinate filters
#' @param crs Spatial reference for filter and output
#' @param aggregate Aggregation operators to apply
#'
#'
#' @examples
#' Query(theDatasource = "asp_5df5a74a-13d7-455a-a776-ed6c6db72644")
#'
#' @import tidyverse
#' @import httr
#' @import rjson
#'
#' @export


createQuery <- function(theDatasource = NULL,
                        theToken =NULL,
                        parameters = "{}",
                        description = "null",
                        variables = "null",
                        timefilter = "null",
                        geofilter = "null",
                        coordfilter = "null",
                        crs = "null",
                        aggregate = "null"){

  if(is.null(theDatasource)){
    stop("Datasource value must be provided")
  }

    # Initialise
    service = "https://datamesh.oceanum.io"
    data_gateway = "https://gateway.datamesh.oceanum.io/data/"
    url <- 'https://gateway.datamesh.oceanum.io/oceanql/stage/'

    stageHeader <- c('Authorization' = paste('Token', theToken),
                     'X-DATAMESH-TOKEN' = theToken)

    gatewayHeader = c("Accept" = "application/json",
                      'Authorization' = paste('Token', theToken),
                      'X-DATAMESH-TOKEN' = theToken)

    Query <- list(
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
      )
    )

    # Initialise a data stucture for createQuery GET call
    theFilter <- paste0('{"datasource":', theDatasource, ",",
                        '"parameters":', parameters, ",",
                        '"description":', description, ",",
                        '"variables":', variables, ",",
                        '"timefilter":', timefilter, ",",
                        '"geofilter":', geofilter, ",",
                        '"coordfilter":', coordfilter, ",",
                        '"crs":', crs, ",",
                        '"aggregate":', aggregate, "}")

    metadata <- POST(url, add_headers(stageHeader), body = theFilter, encode = 'json') %>%
                content("parsed")

    result <- GET(paste0(data_gateway, theDatasource),
                add_headers(gatewayHeader))

    result <- rawToChar(result$content) %>%
              fromJSON()

    data <- lapply(result$data_vars, function(x) x$data) %>%
             bind_rows()

    if(!rlang::is_empty(result$coords$index$attrs)){

      coords <- lapply(result$coords$index$attrs, function(x) x$data) %>%
                bind_rows()

      if(nrows(data) == nrows(coords)){

      data <- bind_cols(data, coords)

      }

    }


    return(data)

}
