#' Postcode geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link point geometries from GeoNames' postal codes
#' database (\url{https://www.geonames.org/postal-codes/}). Provides no functions for
#' validating and parsing as postal codes are usually pretty generic numbers.
#'
#' Requires the \href{https://docs.ropensci.org/geonames/}{\code{geonames}}
#' package to be installed.
#'
#' @inheritParams enrich
#' @param username Username for the GeoNames API. Necessary for any request.
#'   For details, see the
#'   \href{https://www.geonames.org/export/web-services.html}{documentation}
#' @param ... Further arguments passed to the `postalCodeSearch` endpoint
#'   of GeoNames. For details, see the
#'   \href{https://www.geonames.org/export/web-services.html}{documentation}.
#'
#' @returns
#' An sf dataframe containing \code{.data} and an added geometry column.
#'
#' @export
#' @name postcode
#'
#' @examples \donttest{try(enrich("50667", linker = "postcode"))}
postcode_link <- function(.data, id_col, username, ...) {
  if (missing(username)) {
    cli::cli_abort(c(
      "If `linker = \"postcode\"`, the `username` argument must be provided.",
      "i" = paste(
        "You must be registered for the GeoNames",
        "({.url https://www.geonames.org/export/web-services.html})",
        "web service to query postcodes."
      )
    ))
  }

  reference <- lapply(.data[[id_col]], function(id) {
    args <- c(list(postalcode = id, maxRows = 1), ...)
    res <- suppressWarnings(do.call(geonames::GNpostalCodeSearch, args))
    if (is.null(res$lng) || is.null(res$lat)) {
      sf::st_point()
    } else {
      sf::st_point(as.numeric(c(res$lng, res$lat)))
    }
  })
  reference <- sf::st_sf(
    postcode = .data[[id_col]],
    geometry = sf::st_as_sfc(reference),
    crs = 4326
  )

  left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = "postcode"
  )
}
