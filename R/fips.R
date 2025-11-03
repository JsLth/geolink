#' FIPS geolinker
#' @description
#' Default geolinker registered through \code{\link{register_geolinker}}.
#' These functions are not intended to be used directly but through
#' \code{\link{enrich}}!
#'
#' Contains functions to link FIPS geometries from the US census. Provides no
#' functions for validating and parsing as FIPS codes are usually pretty
#' generic numbers.
#'
#' Requires the \href{https://fipio.justinsingh.me/}{\code{fipio}}
#' package to be installed.
#'
#' @inheritParams enrich
#' @param ... Not used.
#'
#' @returns
#' An sf dataframe containing \code{.data} and an added geometry column.
#'
#' @export
#' @name fips
#'
#' @examplesIf requireNamespace("fipio")
#' \donttest{# Link Alaska
#' enrich("02", linker = "fips")}
fips_link <- function(.data, id_col, ...) {
  check_installed("fipio", "to link FIPS geometries")
  geom <- fipio::fips_geometry(.data[[id_col]])
  reference <- sf::st_sf(fips = .data[[id_col]], geometry = geom)
  left_merge(.data, reference, by.x = id_col, by.y = "fips")
}
