#' NUTS geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link, parse and validate geometries from the
#' Nomenclature of Territorial Units for Statistics (NUTS,
#' \url{https://ec.europa.eu/eurostat/web/nuts/}).
#'
#' Requires the \href{https://ropengov.github.io/giscoR/}{\code{giscoR}}
#' package to be installed.
#'
#' @param ids A character vector of IDs.
#' @inheritParams naturalearth
#' @param country Character vector of unique ISO-3 country codes contained
#'   by \code{ids}. Only geometries within these countries are returned.
#' @param level Vector of geographic levels. Only geometries that adhere to
#'   these levels are returned.
#' @param ... Further arguments passed \code{\link[giscoR]{gisco_get_nuts}}.
#'
#' @returns
#' \code{is_nuts}: A logical vector of length \code{length(ids)}.
#'
#' \code{nuts_parse}: A list with fields \code{country} and \code{level}.
#'
#' \code{nuts_link}: An sf dataframe containing \code{.data} and an added
#' geometry column.
#'
#' @export
#' @name nuts
#'
#' @examplesIf requireNamespace("giscoR")
#' \donttest{# Link Normandy
#' enrich("FRD", linker = "nuts")}
nuts_link <- function(.data, id_col, country, level, ...) {
  for (lvl in level) {
    reference <- giscoR::gisco_get_nuts(
      country = country,
      nuts_level = lvl,
      nuts_id = .data[[id_col]],
      ...
    )[c("NUTS_ID", "geometry")]

    merged <- left_merge(
      .data[level == lvl, , drop = FALSE],
      reference,
      by.x = id_col,
      by.y = "NUTS_ID"
    )
    .data[level == lvl, names(merged)] <- merged
  }

  as_sf_tibble(.data, crs = sf::st_crs(reference))
}


#' @rdname nuts
#' @export
is_nuts <- function(ids) {
  grepl("^[A-Z]{2}[A-Z0-9]{1,3}$", ids)
}


#' @rdname nuts
#' @export
nuts_parse <- function(ids) {
  nuts <- utils::strcapture(
    "^([A-Z]{2})([A-Z0-9])?([A-Z0-9])?([A-Z0-9])?$",
    ids,
    proto = list(country = "", level1 = "", level2 = "", level3 = "")
  )

  nuts$level1 <- nzchar(nuts$level1)
  nuts$level2 <- nzchar(nuts$level2)
  nuts$level3 <- nzchar(nuts$level3)
  level <- nuts$level1 + nuts$level2 + nuts$level3

  list(
    country = unique(nuts$country),
    level = unique(level)
  )
}
