#' Connector
#'
#' Objected used to establish connection with Datamesh service
#'
#' @param token Character string with the Scopus query
#'
#' @examples
#' Connector$new(token = yourToken)
#'
#' @import R6
#' @import tidyverse
#' @import httr2
#' @import rjson
#'
#' @export

Connector <- R6::R6Class(
  "Connector",

  private = list(
    token = NULL,
    service = NULL,
    gateway = NULL,
    auth_headers = NULL,
    verify = TRUE,
    proto = NULL,
    host = NULL,
    user = NULL,
    # -------- staging --------
    stage_results = function(connector, query_input){
      req <- httr2::request(paste0(private$service, "/oceanql/stage/")) |>
        httr2::req_headers(
          !!!private$auth_headers,
          "Content-Type" = "application/json",
          Accept = "application/json"
        ) |>
        httr2::req_body_json(query_input)
      resp <- httr2::req_perform(req)
      if (httr2::resp_status(resp) != 200) {
        return(NULL)
      }
      httr2::resp_body_json(resp, simplifyVector = TRUE)
      return(Stage$new(httr2::resp_body_json(resp, simplifyVector = TRUE)))
    }
  ),

  public = list(

    # -------- constructor --------
    initialize = function(
    token   = Sys.getenv("DATAMESH_TOKEN"),
    service = "https://datamesh.oceanum.io",
    verify  = TRUE
    ) {
      if (identical(token, "")) {
        stop(
          "A valid token must be supplied or defined in DATAMESH_TOKEN",
          call. = FALSE
        )
      }

      private$token   <- token
      private$service <- service
      private$verify  <- verify

      # parse URL
      parsed <- httr2::url_parse(private$service)
      private$proto <- parsed$scheme
      private$host  <- parsed$hostname

      # auth headers
      if (startsWith(token, "Bearer ")) {
        private$auth_headers <- c(
          Authorization = token
        )
      } else {
        private$auth_headers <- c(
          Authorization      = paste("Token", token),
          "X-DATAMESH-TOKEN"  = token
        )
      }

      private$gateway <- private$service

      # setup session
      private$user <- private$session
    },

    # -------- simple accessors --------
    get_host = function() {
      private$host
    },

    check_info = function() {
      NA_real_
    },

    # -------- status --------
    status = function() {
      req <- request(private$gateway) |>
        req_headers(!!!private$auth_headers) |>
        req_method("GET") |>
        req_options(ssl_verifypeer = private$verify)

      resp <- req_perform(req)

      if (resp_status(resp) == 200) {
        message(sprintf(
          "Datamesh connector created for %s",
          private$host
        ))
        TRUE
      } else {
        FALSE
      }
    },

    # -------- catalog --------
    get_catalog = function(
    search     = "",
    timefilter = NULL,
    geofilter  = NULL,
    limit      = NA_integer_
    ) {

      query <- list()
      if (nzchar(search)) query$search <- search
      if (!is.na(limit))  query$limit  <- limit

      if (!is.null(timefilter)) {
        in_trange <- private$format_timefilter(timefilter)
        if (nzchar(in_trange)) query$in_trange <- in_trange
      }

      if (!is.null(geofilter)) {
        if (is.character(geofilter)) {
          query$geom_intersects <- geofilter
        } else if (is.list(geofilter) && !is.null(geofilter$wkt)) {
          query$geom_intersects <- geofilter$wkt
        } else {
          stop("Invalid geofilter")
        }
      }

      req <- httr2::request(
        paste0(private$proto, "://", private$host, "/datasource/")
      ) |>
        httr2::req_headers(!!!private$auth_headers) |>
        httr2::req_url_query(!!!query) |>
        httr2::req_method("GET")

      resp <- httr2::req_perform(req)

      if (httr2::resp_status(resp) != 200) {
        stop("Failed to retrieve catalog")
      }

      content <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      return(Catalog$new(content, self))
    },

    # -------- datasource metadata --------
    get_datasource = function(datasource_id) {
      req <- httr2::request(
        paste0(private$proto, "://", private$host, "/datasource/", datasource_id)
      ) |>
        httr2::req_headers(!!!private$auth_headers)

      resp <- httr2::req_perform(req)

      status <- httr2::resp_status(resp)
      if (status == 404) stop("Datasource not found")
      if (status == 401) stop("Unauthorized")
      if (status != 200) stop("Request failed")

      data <- httr2::resp_body_json(resp, simplifyVector = TRUE)

      props <- data$properties
      props$id   <- datasource_id
      props$geom <- data$geometry
      props$bbox <- data$bbox

      Datasource$new(props,self)
    },

    # -------- load datasource --------
    load_datasource = function(
    datasource_id,
    size_limit = 1e9,
    row_limit  = 2e6
    ) {

      stage <- private$stage_results$new(list(datasource = datasource_id))

      if (stage$size > size_limit) {
        stop("Datasource too large for memory")
      }

      if (stage$dlen > row_limit) {
        warning("Row limit exceeded")
      }

      req <- httr2::request(
        paste0(private$gateway, "/data/", datasource_id)
      ) |>
        httr2::req_headers(
          !!!private$auth_headers,
          Accept = "application/json"
        )

      resp <- httr2::req_perform(req)

      if (httr2::resp_status(resp) != 200) {
        stop("Failed to load datasource")
      }

      httr2::resp_body_json(resp, simplifyVector = TRUE)
    },

    # -------- query --------
    query = function(
    query_input,
    query_size_limit = 1e9,
    row_limit = 2e6
    ) {

      stage <- private$stage_results(self, query_input)

      if (is.null(stage)) stop("No data returned")

      if (stage$size > query_size_limit)
        stop("Query size limit exceeded")

      if (stage$dlen > row_limit)
        warning("Row limit exceeded")

      req <- httr2::request(paste0(private$service, "/oceanql/")) |>
        httr2::req_headers(
          !!!private$auth_headers,
          "Content-Type" = "application/json",
          Accept = "application/json"
        ) |>
        httr2::req_body_json(query_input)

      resp <- httr2::req_perform(req)

      httr2::resp_body_json(resp, simplifyVector = TRUE)
    }
  )
)


