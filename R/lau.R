#' LAU geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link, parse and validate geometries from Eurostat's
#' Local Administrative Units (LAU,
#' \url{https://ec.europa.eu/eurostat/web/nuts/local-administrative-units}).
#'
#' Requires the \href{https://ropengov.github.io/giscoR/}{\code{giscoR}}
#' package to be installed.
#'
#' @param ids A character vector of IDs.
#' @inheritParams naturalearth
#' @param country Character vector of unique ISO-3 country codes contained
#'   by \code{ids}. Only geometries within these countries are returned.
#' @param ... Further arguments passed \code{\link[giscoR]{gisco_get_lau}}.
#'
#' @returns
#' \code{is_lau}: A logical vector of length \code{length(ids)}.
#'
#' \code{lau_parse}: A list with fields \code{country} and \code{level}.
#'
#' \code{lau_link}: An sf dataframe containing \code{.data} and an added
#' geometry column.
#'
#' @export
#' @name lau
#'
#' @examplesIf requireNamespace("giscoR")
#' \donttest{# Link Normandy
#' enrich("FRD", linker = "nuts")}
lau_link <- function(.data, id_col, country, ...) {
  reference <- giscoR::gisco_get_lau(
    country = country,
    gisco_id = .data[[id_col]],
    ...
  )[c("GISCO_ID", "geometry")]

  reference <- sf::st_sf(
    sf::st_drop_geometry(reference),
    geometry = sf::st_geometry(reference)
  )

  .data <- left_merge(
    .data,
    reference,
    by.x = id_col,
    by.y = "GISCO_ID"
  )
}


#' @rdname lau
#' @export
is_lau <- function(ids) {
  grepl("[A-Z]{2}_[0-9]+", ids)
}


#' @rdname lau
#' @export
lau_parse <- function(ids) {
  lau <- utils::strcapture(
    "([A-Z]{2})_[0-9]+",
    ids,
    proto = list(country = "")
  )

  list(country = unique(lau$country))
}
